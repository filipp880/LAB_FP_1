package backup

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.zip.ZipFile
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

case class VerifyResult(ok: Int, damaged: List[String], missing: List[String])
case class RestoreResult(restored: Int, skipped: Int, problems: List[AppError])

object Container:
  def now(): String =
    val time = LocalDateTime.now()
    val format = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    time.format(format)

  def readMeta(path: Path): Either[AppError, StorageMeta] = ContainerFile.readMeta(path)

  private def keyFor(meta: StorageMeta, key: Option[String]): Either[AppError, Option[String]] =
    if !meta.encrypted then Right(None)
    else key match
      case None => Left(AppError.KeyRequired)
      case Some("") => Left(AppError.KeyRequired)
      case Some(value) =>
        if meta.keyCheck.contains(Crypto.keyCheck(value, meta.name)) then Right(Some(value))
        else Left(AppError.WrongKey)

  def open(path: Path, key: Option[String]): Either[AppError, StorageMeta] =
    readMeta(path).flatMap { meta =>
      keyFor(meta, key).map(_ => meta)
    }

  def create(
      path: Path,
      name: String,
      encrypted: Boolean,
      key: Option[String],
      sources: List[Source]
  ): Either[AppError, StorageMeta] =
    if Files.exists(path) then
      return Left(AppError.FileExists(path.toString))

    if name.trim.isEmpty then
      return Left(AppError.InvalidInput("Название хранилища пустое"))

    var keyCheck: Option[String] = None
    if encrypted then
      key match
        case None => return Left(AppError.KeyRequired)
        case Some(value) =>
          if value.isEmpty then
            return Left(AppError.KeyRequired)
          keyCheck = Some(Crypto.keyCheck(value, name))

    val meta = StorageMeta(
      name = name,
      containerPath = path,
      encrypted = encrypted,
      keyCheck = keyCheck,
      created = now(),
      sources = sources,
      versions = List.empty[VersionInfo]
    )
    val fileContents = Map.empty[String, Array[Byte]]
    val writeResult = ContainerFile.write(meta, fileContents, key = None, replacing = false)
    writeResult.map(result => meta)

  def save(meta: StorageMeta, key: Option[String]): Either[AppError, Unit] =
    if Files.notExists(meta.containerPath) then Left(AppError.FileMissing(meta.containerPath.toString))
    else keyFor(meta, key).flatMap(value => ContainerFile.write(meta, Map.empty, value, true))

  // Идём от последней версии назад и собираем разные хеши одного файла.
  private def latestHashes(path: String, versions: List[VersionInfo], maximum: Int): List[String] =
    val hashes = ArrayBuffer.empty[String]
    var index = versions.length - 1
    while index >= 0 && hashes.length < maximum do
      val file = versions(index).snapshot.get(path)
      val hash = file.flatMap(record => record.hash)
      hash match
        case Some(value) =>
          if !hashes.contains(value) then hashes += value
        case None => ()
      index -= 1
    hashes.toList

  private def retain(meta: StorageMeta, maximum: Int): StorageMeta =
    val versions = meta.versions.map { version =>
      val snapshot = version.snapshot.filter { case (path, ref) =>
        ref.hash match
          case None => true
          case Some(hash) =>
            val allowed = latestHashes(path, meta.versions, maximum)
            allowed.contains(hash)
      }
      val result =
        if snapshot.size < version.snapshot.size then "partial: старые данные удалены по лимиту"
        else version.result
      version.copy(snapshot = snapshot, result = result)
    }
    meta.copy(versions = versions)

  private def readSource(meta: StorageMeta, cfg: Config, file: WalkedFile): Either[AppError, Array[Byte]] =
    Scanner.resolveRel(meta, file.rel) match
      case None => Left(AppError.FileChanged(file.rel))
      case Some(path) =>
        try
          val protectedPaths = List(Paths.get(cfg.keyDirectory), Paths.get(cfg.logFile), Config.defaultPath, meta.containerPath)
          val absolutePath = path.toAbsolutePath.normalize
          val protectedFile = protectedPaths.exists { item =>
            val protectedPath = item.toAbsolutePath.normalize
            absolutePath.startsWith(protectedPath)
          }
          if protectedFile then Left(AppError.InvalidInput("Служебные файлы и ключи не включаются в резервную копию"))
          else
            val sizeBefore = Files.size(path)
            val timeBefore = Files.getLastModifiedTime(path).toMillis
            val bytes = Files.readAllBytes(path)
            val sizeAfter = Files.size(path)
            val timeAfter = Files.getLastModifiedTime(path).toMillis
            Crypto.hash(cfg.hashAlgorithm, bytes).flatMap { hash =>
              var changed = false
              if sizeBefore != sizeAfter then changed = true
              if timeBefore != timeAfter then changed = true
              if sizeAfter != file.size then changed = true
              if timeAfter != file.mtime then changed = true
              if hash != file.hash then changed = true
              if changed then Left(AppError.FileChanged(path.toString))
              else Right(bytes)
            }
        catch case NonFatal(_) => Left(AppError.ReadFailed(path.toString))

  private def checkDeletion(meta: StorageMeta, record: LastRecord): Either[AppError, LastRecord] =
    val source = record.srcIndex.flatMap(index => meta.sources.lift(index))
    source match
      case None => Left(AppError.FileChanged(record.rel))
      case Some(value) if !value.available => Left(AppError.SourceMissing(value.path))
      case Some(_) =>
        Scanner.resolveRel(meta, record.rel) match
          case None => Left(AppError.FileChanged(record.rel))
          case Some(path) =>
            if Files.notExists(path) then Right(record)
            else if Files.isDirectory(path) then Right(record)
            else Left(AppError.FileChanged(record.rel))

  def backup(meta: StorageMeta, key: Option[String], cfg: Config, plan: ScanPlan,
             comment: Option[String]): Either[AppError, (StorageMeta, BackupResult)] =
    if cfg.maximumVersionsPerFile < 1 then Left(AppError.InvalidConfig("Максимальное количество версий меньше единицы"))
    else keyFor(meta, key).flatMap { actualKey =>
      val changedFiles = plan.report.changed.map { case (_, file) => file }
      val files = plan.report.newFiles ++ changedFiles
      val added = ArrayBuffer.empty[String]
      val modified = ArrayBuffer.empty[String]
      val deleted = ArrayBuffer.empty[String]
      val problems = ArrayBuffer.empty[AppError]
      for problem <- plan.problems do problems += problem
      val failedSources = ArrayBuffer.empty[Int]
      // Начинаем со старого снимка: пропущенный файл сохраняет прежнюю копию.
      val previousSnapshot = meta.lastVersion.map(version => version.snapshot)
      var snapshot = previousSnapshot.getOrElse(Map.empty[String, FileRef])
      var blobs = Map.empty[String, Array[Byte]]
      var bytes = 0L

      def markFailedSource(rel: String): Unit =
        val sourceIndex = Scanner.sourceIndexOf(rel)
        sourceIndex match
          case Some(index) => failedSources += index
          case None => ()

      for file <- files do
        readSource(meta, cfg, file) match
          case Right(content) =>
            val ref = FileRef(hash = Some(file.hash), size = file.size)
            snapshot = snapshot.updated(file.rel, ref)
            blobs = blobs.updated(file.hash, content)
            bytes += file.size
            val isNew = plan.report.newFiles.exists(planned => planned.rel == file.rel)
            if isNew then added += file.rel
            else modified += file.rel
          case Left(error) =>
            problems += error
            markFailedSource(file.rel)

      for record <- plan.report.deleted do
        checkDeletion(meta, record) match
          case Right(value) =>
            snapshot = snapshot.updated(value.rel, FileRef(hash = None, size = 0L))
            deleted += value.rel
          case Left(error) =>
            problems += error
            markFailedSource(record.rel)

      val renamed = ArrayBuffer.empty[(String, String)]
      for (old, fresh) <- plan.report.renamed do
        val oldWasDeleted = deleted.contains(old.rel)
        val newWasSaved = added.contains(fresh.rel)
        if oldWasDeleted then
          if newWasSaved then renamed += ((old.rel, fresh.rel))
      val timestamp = now()
      val number = meta.lastVersion.map(version => version.number + 1).getOrElse(1)
      val status = if problems.isEmpty then "success" else "partial"
      val version = VersionInfo(
        number = number,
        timestamp = timestamp,
        comment = comment,
        result = status,
        added = added.toList,
        modified = modified.toList,
        deleted = deleted.toList,
        renamed = renamed.toList,
        bytes = bytes,
        snapshot = snapshot
      )

      val updatedSources = meta.sources.zipWithIndex.map { case (source, index) =>
        var successful = plan.scannedSources.contains(index)
        if failedSources.contains(index) then successful = false
        if !source.available then successful = false
        if successful then
          source.copy(lastBackup = Some(timestamp))
        else source
      }
      val history = ArrayBuffer.empty[VersionInfo]
      for savedVersion <- meta.versions do history += savedVersion
      history += version
      val next = meta.copy(versions = history.toList, sources = updatedSources)
      val updated = retain(next, cfg.maximumVersionsPerFile)
      if Files.notExists(meta.containerPath) then Left(AppError.FileMissing(meta.containerPath.toString))
      else
        val writeResult = ContainerFile.write(updated, blobs, actualKey, replacing = true)
        writeResult.map { writeCompleted =>
          val result = BackupResult(
            version = version,
            added = added.size,
            updated = modified.size,
            deleted = deleted.size,
            skipped = problems.size,
            bytes = bytes,
            problems = problems.toList
          )
          (updated, result)
        }
    }

  private def plainBlob(zip: ZipFile, hash: String, key: Option[String]): Either[AppError, Array[Byte]] =
    val storedResult = ContainerFile.entryBytes(zip, ContainerFile.BlobPrefix + hash)
    val decodedResult = storedResult.flatMap { stored =>
      key match
        case None => Right(stored)
        case Some(value) => Crypto.decrypt(value, stored)
    }
    val checkedResult = decodedResult.flatMap { bytes =>
      val algorithm = if hash.length == 32 then "MD5" else "SHA-256"
      val hashResult = Crypto.hash(algorithm, bytes)
      hashResult.flatMap { actual =>
        if actual == hash then Right(bytes)
        else Left(AppError.DamagedStorage(s"Повреждён блок $hash"))
      }
    }
    checkedResult

  private def versionOf(meta: StorageMeta, number: Int): Either[AppError, VersionInfo] =
    val version = meta.versions.find(version => version.number == number)
    version match
      case Some(value) => Right(value)
      case None => Left(AppError.VersionNotFound(number))

  // Ссылка — путь-перенаправление. Восстанавливать через неё нельзя.
  private def containsLink(path: Path, root: Path): Boolean =
    var current = path
    var found = Files.isSymbolicLink(current)
    while current != root && !found do
      current = current.getParent
      found = Files.isSymbolicLink(current)
    found

  private def safeTarget(dest: Path, rel: String): Either[AppError, Path] =
    try
      val root = dest.toAbsolutePath.normalize
      val target = root.resolve(rel).normalize
      var unsafe = false
      if Scanner.sourceIndexOf(rel).isEmpty then unsafe = true
      if !target.startsWith(root) then unsafe = true
      if target == root then unsafe = true
      if unsafe then
        Left(AppError.DamagedStorage(s"Путь выходит за каталог восстановления: $rel"))
      else if containsLink(target, root) then
        Left(AppError.DamagedStorage(s"В пути восстановления есть ссылка: $rel"))
      else Right(target)
    catch case NonFatal(_) => Left(AppError.InvalidPath("Не удалось получить путь восстановления: " + rel))

  def restoreConflicts(meta: StorageMeta, number: Int, dest: Path): Either[AppError, List[String]] =
    versionOf(meta, number).flatMap { version =>
      val conflicts = ArrayBuffer.empty[String]
      var problem: Option[AppError] = None
      for (rel, ref) <- version.snapshot do
        if ref.hash.nonEmpty && problem.isEmpty then
          safeTarget(dest, rel) match
            case Left(error) => problem = Some(error)
            case Right(path) =>
              if Files.exists(path) then conflicts += rel
      problem match
        case Some(error) => Left(error)
        case None =>
          val paths = conflicts.toList
          Right(paths.sorted)
    }

  private def restoreFile(zip: ZipFile, key: Option[String], dest: Path,
                          rel: String, hash: String, overwrite: Boolean): Either[AppError, Boolean] =
    safeTarget(dest, rel).flatMap { target =>
      if Files.exists(target) && !overwrite then Right(false)
      else plainBlob(zip, hash, key).flatMap { bytes =>
        try
          // Java API создаёт недостающие родительские каталоги файла.
          Files.createDirectories(target.getParent)
          val temp = Files.createTempFile(target.getParent, "restore-", ".tmp")
          try
            Files.write(temp, bytes)
            if overwrite then Files.move(temp, target, StandardCopyOption.REPLACE_EXISTING)
            else Files.move(temp, target)
            Right(true)
          finally Files.deleteIfExists(temp)
        catch case NonFatal(_) => Left(AppError.RestoreFailed(rel))
      }
    }

  def restore(meta: StorageMeta, key: Option[String], versionNumber: Int,
              destDir: Path, overwrite: List[String] = Nil,
              skipExisting: Boolean = true): Either[AppError, RestoreResult] =
    keyFor(meta, key).flatMap { actualKey =>
      versionOf(meta, versionNumber).flatMap { version =>
        if !Files.isDirectory(destDir) then
          Left(AppError.DestinationUnavailable(destDir.toString))
        else
          try
            val zip = new ZipFile(meta.containerPath.toFile)
            try
              var restored = 0
              var skipped = 0
              val problems = ArrayBuffer.empty[AppError]
              val entries = version.snapshot.toList
              val files = entries.sortBy { case (rel, _) => rel }
              for (rel, ref) <- files do
                ref.hash match
                  case None => ()
                  case Some(hash) =>
                    var replace = !skipExisting
                    if overwrite.contains(rel) then replace = true
                    restoreFile(zip, actualKey, destDir, rel, hash, replace) match
                      case Right(true) => restored += 1
                      case Right(false) => skipped += 1
                      case Left(error) =>
                        skipped += 1
                        problems += error
              Right(RestoreResult(restored = restored, skipped = skipped, problems = problems.toList))
            finally zip.close()
          catch case NonFatal(_) => Left(AppError.ReadFailed(meta.containerPath.toString))
      }
    }

  def verify(meta: StorageMeta, key: Option[String]): Either[AppError, VerifyResult] =
    keyFor(meta, key).flatMap { actualKey =>
      try
        val zip = new ZipFile(meta.containerPath.toFile)
        try
          val hashes = ArrayBuffer.empty[String]
          for version <- meta.versions do
            for file <- version.snapshot.values do
              file.hash match
                case Some(hash) =>
                  if !hashes.contains(hash) then hashes += hash
                case None => ()
          var ok = 0
          val missing = ArrayBuffer.empty[String]
          val damaged = ArrayBuffer.empty[String]
          for hash <- hashes do
            val entry = Option(zip.getEntry(ContainerFile.BlobPrefix + hash))
            if entry.isEmpty then missing += hash
            else plainBlob(zip, hash, actualKey) match
              case Right(_) => ok += 1
              case Left(_) => damaged += hash
          Right(VerifyResult(ok = ok, damaged = damaged.toList, missing = missing.toList))
        finally zip.close()
      catch case NonFatal(_) => Left(AppError.ReadFailed(meta.containerPath.toString))
    }
