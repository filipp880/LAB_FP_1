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
  private case class SavedFile(file: WalkedFile, bytes: Array[Byte])

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
      versions = List.empty[VersionInfo],
      fileVersions = Map.empty[String, List[Int]]
    )
    val fileContents = Map.empty[String, Array[Byte]]
    val writeResult = ContainerFile.write(meta, fileContents, key = None, replacing = false)
    writeResult.map(result => meta)

  def save(meta: StorageMeta, key: Option[String]): Either[AppError, Unit] =
    if Files.notExists(meta.containerPath) then Left(AppError.FileMissing(meta.containerPath.toString))
    else keyFor(meta, key).flatMap(value => ContainerFile.write(meta, Map.empty, value, true))

  // Идём от последней версии назад и собираем разные хеши одного файла.
  private def latestHashes(path: String, versions: List[VersionInfo], maximum: Int): List[String] =
    var hashes = List.empty[String]
    var index = versions.length - 1
    while index >= 0 && hashes.length < maximum do
      val file = versions(index).snapshot.get(path)
      val hash = file.flatMap(_.hash)
      hash match
        case Some(value) =>
          if !hashes.contains(value) then hashes = hashes :+ value
        case None => ()
      index -= 1
    hashes

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
    var paths = List.empty[String]
    for version <- versions do
      for path <- version.snapshot.keys do
        if !paths.contains(path) then paths = paths :+ path

    var fileVersions = Map.empty[String, List[Int]]
    for path <- paths do
      var numbers = List.empty[Int]
      for version <- versions.reverse do
        val wasChanged = version.added.contains(path) || version.modified.contains(path)
        val file = version.snapshot.get(path)
        val hash = file.flatMap(record => record.hash)
        val hasContent = hash.nonEmpty
        if wasChanged && hasContent && numbers.length < maximum then
          numbers = version.number :: numbers
      if numbers.nonEmpty then fileVersions = fileVersions.updated(path, numbers)
    meta.copy(versions = versions, fileVersions = fileVersions)

  private def readSource(meta: StorageMeta, cfg: Config, file: WalkedFile): Either[AppError, SavedFile] =
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
              if sizeBefore != sizeAfter || timeBefore != timeAfter ||
                  sizeAfter != file.size || timeAfter != file.mtime || hash != file.hash then
                Left(AppError.FileChanged(path.toString))
              else Right(SavedFile(file, bytes))
            }
        catch case NonFatal(_) => Left(AppError.ReadFailed(path.toString))

  private def checkDeletion(meta: StorageMeta, record: LastRecord): Either[AppError, LastRecord] =
    val source = record.srcIndex.flatMap(index => meta.sources.lift(index))
    source match
      case None => Left(AppError.FileChanged(record.rel))
      case Some(value) if !value.available => Left(AppError.SourceMissing(value.path))
      case Some(_) =>
        Scanner.resolveRel(meta, record.rel) match
          case Some(path) if Files.notExists(path) || Files.isDirectory(path) => Right(record)
          case _ => Left(AppError.FileChanged(record.rel))

  def backup(meta: StorageMeta, key: Option[String], cfg: Config, plan: ScanPlan,
             comment: Option[String]): Either[AppError, (StorageMeta, BackupResult)] =
    if cfg.maximumVersionsPerFile < 1 then Left(AppError.InvalidConfig("Максимальное количество версий меньше единицы"))
    else keyFor(meta, key).flatMap { actualKey =>
      val changedFiles = plan.report.changed.map { case (_, file) => file }
      val files = plan.report.newFiles ++ changedFiles
      var saved = List.empty[SavedFile]
      var deleted = List.empty[LastRecord]
      var problems = plan.problems
      val failedSources = ArrayBuffer.empty[Int]

      def markFailedSource(rel: String): Unit =
        val sourceIndex = Scanner.sourceIndexOf(rel)
        sourceIndex match
          case Some(index) => failedSources += index
          case None => ()

      for file <- files do
        readSource(meta, cfg, file) match
          case Right(value) => saved = saved :+ value
          case Left(error) =>
            problems = problems :+ error
            markFailedSource(file.rel)

      for record <- plan.report.deleted do
        checkDeletion(meta, record) match
          case Right(value) => deleted = deleted :+ value
          case Left(error) =>
            problems = problems :+ error
            markFailedSource(record.rel)

      // Начинаем со старого снимка: пропущенный файл сохраняет прежнюю копию.
      var snapshot = meta.lastVersion.map(_.snapshot).getOrElse(Map.empty)
      for item <- saved do
        val ref = FileRef(Some(item.file.hash), item.file.size)
        snapshot = snapshot.updated(item.file.rel, ref)
      for record <- deleted do
        snapshot = snapshot.updated(record.rel, FileRef(None, 0L))
      val newPaths = plan.report.newFiles.map(file => file.rel)
      val savedPaths = saved.map(item => item.file.rel)
      val added = savedPaths.filter(path => newPaths.contains(path))
      val modified = savedPaths.filter(path => !newPaths.contains(path))
      var renamed = List.empty[(String, String)]
      for (old, fresh) <- plan.report.renamed do
        val oldWasDeleted = deleted.exists(record => record.rel == old.rel)
        val newWasSaved = added.contains(fresh.rel)
        if oldWasDeleted && newWasSaved then
          renamed = renamed :+ ((old.rel, fresh.rel))
      val timestamp = now()
      val number = meta.lastVersion.map(_.number + 1).getOrElse(1)
      val sizes = saved.map(item => item.file.size)
      val bytes = sizes.sum
      val version = VersionInfo(number, timestamp, comment,
        if problems.isEmpty then "success" else "partial",
        added, modified, deleted.map(_.rel), renamed, bytes, snapshot)

      val sources = meta.sources.zipWithIndex.map { case (source, index) =>
        var successful = plan.scannedSources.contains(index)
        if failedSources.contains(index) then successful = false
        if !source.available then successful = false
        if successful then
          source.copy(lastBackup = Some(timestamp))
        else source
      }
      val updated = retain(meta.copy(versions = meta.versions :+ version, sources = sources), cfg.maximumVersionsPerFile)
      var blobs = Map.empty[String, Array[Byte]]
      for item <- saved do
        blobs = blobs.updated(item.file.hash, item.bytes)
      if Files.notExists(meta.containerPath) then Left(AppError.FileMissing(meta.containerPath.toString))
      else ContainerFile.write(updated, blobs, actualKey, true).map { _ =>
        val result = BackupResult(version, added.size, modified.size, deleted.size, problems.size, bytes, problems)
        (updated, result)
      }
    }

  private def plainBlob(zip: ZipFile, hash: String, key: Option[String]): Either[AppError, Array[Byte]] =
    ContainerFile.entryBytes(zip, ContainerFile.BlobPrefix + hash).flatMap { stored =>
      val decoded = key match
        case None => Right(stored)
        case Some(value) => Crypto.decrypt(value, stored)
      decoded.flatMap { bytes =>
        val algorithm = if hash.length == 32 then "MD5" else "SHA-256"
        Crypto.hash(algorithm, bytes).flatMap { actual =>
          if actual == hash then Right(bytes)
          else Left(AppError.DamagedStorage(s"Повреждён блок $hash"))
        }
      }
    }

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
      if Scanner.sourceIndexOf(rel).isEmpty || !target.startsWith(root) || target == root then
        Left(AppError.DamagedStorage(s"Путь выходит за каталог восстановления: $rel"))
      else if containsLink(target, root) then
        Left(AppError.DamagedStorage(s"В пути восстановления есть ссылка: $rel"))
      else Right(target)
    catch case NonFatal(_) => Left(AppError.InvalidPath("Не удалось получить путь восстановления: " + rel))

  def restoreConflicts(meta: StorageMeta, number: Int, dest: Path): Either[AppError, List[String]] =
    versionOf(meta, number).flatMap { version =>
      var conflicts = List.empty[String]
      var problem: Option[AppError] = None
      for (rel, ref) <- version.snapshot do
        if ref.hash.nonEmpty && problem.isEmpty then
          safeTarget(dest, rel) match
            case Left(error) => problem = Some(error)
            case Right(path) =>
              if Files.exists(path) then conflicts = conflicts :+ rel
      problem match
        case Some(error) => Left(error)
        case None => Right(conflicts.sorted)
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
              var problems = List.empty[AppError]
              val entries = version.snapshot.toList
              val files = entries.sortBy { case (rel, _) => rel }
              for (rel, ref) <- files do
                ref.hash.foreach { hash =>
                  val replace = !skipExisting || overwrite.contains(rel)
                  restoreFile(zip, actualKey, destDir, rel, hash, replace) match
                    case Right(true) => restored += 1
                    case Right(false) => skipped += 1
                    case Left(error) =>
                      skipped += 1
                      problems = problems :+ error
                }
              Right(RestoreResult(restored, skipped, problems))
            finally zip.close()
          catch case NonFatal(_) => Left(AppError.ReadFailed(meta.containerPath.toString))
      }
    }

  def verify(meta: StorageMeta, key: Option[String]): Either[AppError, VerifyResult] =
    keyFor(meta, key).flatMap { actualKey =>
      try
        val zip = new ZipFile(meta.containerPath.toFile)
        try
          var hashes = List.empty[String]
          for version <- meta.versions do
            for file <- version.snapshot.values do
              file.hash.foreach { hash =>
                if !hashes.contains(hash) then hashes = hashes :+ hash
              }
          var ok = 0
          var missing = List.empty[String]
          var damaged = List.empty[String]
          for hash <- hashes do
            val entry = Option(zip.getEntry(ContainerFile.BlobPrefix + hash))
            if entry.isEmpty then missing = missing :+ hash
            else plainBlob(zip, hash, actualKey) match
              case Right(_) => ok += 1
              case Left(_) => damaged = damaged :+ hash
          Right(VerifyResult(ok, damaged, missing))
        finally zip.close()
      catch case NonFatal(_) => Left(AppError.ReadFailed(meta.containerPath.toString))
    }
