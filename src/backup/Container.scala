package backup

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, Paths, StandardCopyOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.zip.{ZipEntry, ZipException, ZipFile, ZipOutputStream}
import scala.jdk.CollectionConverters.*
import scala.util.Using

final case class VerifyResult(ok: Int, damaged: List[String], missing: List[String])
final case class RestoreResult(restored: Int, skipped: Int, problems: List[AppError])

object Container:
  private val MetaEntry = "meta/filerecords.json"
  private val BlobPrefix = "blobs/"
  def now(): String = LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))

  private def withZip[A](path: Path)(body: ZipFile => Either[AppError, A]): Either[AppError, A] =
    Boundary.attempt("Чтение контейнера") {
      try Using.resource(new ZipFile(path.toFile))(body)
      catch case _: ZipException => Left(AppError.DamagedStorage("Неверный ZIP-контейнер"))
    }.flatMap(identity)

  private def entryBytes(zip: ZipFile, name: String): Either[AppError, Array[Byte]] =
    Option(zip.getEntry(name)).toRight(AppError.DamagedStorage(s"Отсутствует запись $name")).flatMap { entry =>
      Boundary.attempt("Чтение записи контейнера") {
        Using.resource(zip.getInputStream(entry))(_.readAllBytes())
      }
    }

  def readMeta(path: Path): Either[AppError, StorageMeta] =
    withZip(path) { zip =>
      entryBytes(zip, MetaEntry).flatMap { bytes =>
        Boundary.attempt("Разбор метаданных")(ujson.read(new String(bytes, UTF_8)))
          .left.map(_ => AppError.DamagedStorage("Неверный JSON метаданных"))
          .flatMap(StorageMeta.fromJson(_, path))
      }
    }

  private def keyFor(meta: StorageMeta, key: Option[String]): Either[AppError, Option[String]] =
    if !meta.encrypted then Right(None)
    else key.filter(_.nonEmpty).toRight(AppError.KeyRequired).flatMap { value =>
      if meta.keyCheck.contains(Crypto.keyCheck(value, meta.name)) then Right(Some(value))
      else Left(AppError.WrongKey)
    }

  def open(path: Path, key: Option[String]): Either[AppError, StorageMeta] =
    readMeta(path).flatMap(meta => keyFor(meta, key).map(_ => meta))

  private def move(temp: Path, target: Path): Unit =
    try Files.move(temp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch case _: AtomicMoveNotSupportedException =>
      Files.move(temp, target, StandardCopyOption.REPLACE_EXISTING)

  // Замена целевого файла выполняется после завершения записи временного.
  private def replaceFile(target: Path, overwrite: Boolean)(write: Path => Unit): Unit =
    val temp = Files.createTempFile(target.getParent, target.getFileName.toString, ".tmp")
    try
      write(temp)
      if overwrite then move(temp, target) else Files.move(temp, target)
    finally Files.deleteIfExists(temp)

  // Набор ZIP-блоков определяется ссылками из сохранённых снимков.
  private def writeZip(meta: StorageMeta, blobs: Map[String, Array[Byte]],
                       key: Option[String], replacing: Boolean = true): Either[AppError, Unit] =
    val wanted = meta.versions.flatMap(_.snapshot.values.flatMap(_.hash)).toSet
    val prepared = blobs.filter(entry => wanted(entry._1))
      .foldLeft[Either[AppError, Map[String, Array[Byte]]]](Right(Map.empty)) { case (result, (hash, plain)) =>
        for
          all <- result
          bytes <- key.fold[Either[AppError, Array[Byte]]](Right(plain))(Crypto.encrypt(_, plain))
        yield all.updated(hash, bytes)
    }
    prepared.flatMap { extra => Boundary.attempt("Запись контейнера") {
      val target = meta.containerPath.toAbsolutePath
      replaceFile(target, replacing) { temp =>
        Using.resource(new ZipOutputStream(Files.newOutputStream(temp))) { out =>
          val copied = scala.collection.mutable.Set.empty[String]
          if replacing && Files.exists(target) then
            Using.resource(new ZipFile(target.toFile)) { old =>
              old.entries.asScala.filter(entry => entry.getName.startsWith(BlobPrefix)).foreach { entry =>
                val hash = entry.getName.stripPrefix(BlobPrefix)
                if wanted(hash) && !extra.contains(hash) && !copied(hash) then
                  out.putNextEntry(new ZipEntry(entry.getName))
                  Using.resource(old.getInputStream(entry))(_.transferTo(out))
                  out.closeEntry()
                  copied += hash
              }
            }
          extra.toList.sortBy(_._1).foreach { case (hash, bytes) =>
            out.putNextEntry(new ZipEntry(BlobPrefix + hash))
            out.write(bytes)
            out.closeEntry()
          }
          out.putNextEntry(new ZipEntry(MetaEntry))
          out.write(ujson.write(StorageMeta.toJson(meta), indent = 2).getBytes(UTF_8))
          out.closeEntry()
        }
      }
    }}

  def create(path: Path, name: String, encrypted: Boolean, key: Option[String],
             sources: List[Source]): Either[AppError, StorageMeta] =
    if Files.exists(path) then Left(AppError.FileExists(path.toString))
    else if name.trim.isEmpty then Left(AppError.InvalidInput("Название хранилища пустое"))
    else if encrypted && key.forall(_.isEmpty) then Left(AppError.KeyRequired)
    else
      val meta = StorageMeta(name, path, encrypted,
        if encrypted then key.map(Crypto.keyCheck(_, name)) else None,
        now(), sources, Nil, Map.empty)
      writeZip(meta, Map.empty, None, replacing = false).map(_ => meta)

  def save(meta: StorageMeta, key: Option[String]): Either[AppError, Unit] =
    if Files.notExists(meta.containerPath) then Left(AppError.FileMissing(meta.containerPath.toString))
    else keyFor(meta, key).flatMap(writeZip(meta, Map.empty, _))

  // Ограничение истории содержимого применяется отдельно к каждому пути.
  private def retain(meta: StorageMeta, maximum: Int): StorageMeta =
    val paths = meta.versions.flatMap(_.snapshot.keys).toSet
    val allowed = paths.map { rel =>
      val hashes = meta.versions.reverse.flatMap(_.snapshot.get(rel).flatMap(_.hash)).distinct.take(maximum.max(1)).toSet
      rel -> hashes
    }.toMap
    val versions = meta.versions.map { version =>
      val snapshot = version.snapshot.filter { case (rel, ref) => ref.hash.forall(allowed(rel)) }
      version.copy(snapshot = snapshot,
        result = if snapshot.size < version.snapshot.size then "partial: старые данные удалены по лимиту" else version.result)
    }
    val fileVersions = paths.map { rel =>
      val changes = versions.filter(v => (v.added ++ v.modified).contains(rel) && v.snapshot.get(rel).exists(_.hash.nonEmpty))
      rel -> changes.map(_.number).takeRight(maximum.max(1))
    }.filter(_._2.nonEmpty).toMap
    meta.copy(versions = versions, fileVersions = fileVersions)

  // Несоответствие плану сканирования исключает файл из записи новых данных.
  def backup(meta: StorageMeta, key: Option[String], cfg: Config, plan: ScanPlan,
             comment: Option[String]): Either[AppError, (StorageMeta, BackupResult)] =
    keyFor(meta, key).flatMap { actualKey =>
      val files = plan.report.newFiles ++ plan.report.changed.map(_._2)
      val attempted = files.map { file =>
        for
          path <- Scanner.resolveRel(meta, file.rel).toRight(AppError.FileChanged(file.rel))
          _ <- Boundary.attempt("Проверка служебного пути") {
            val protectedPaths = List(Paths.get(cfg.keyDirectory), Paths.get(cfg.logFile), Config.defaultPath, meta.containerPath)
            !protectedPaths.exists(item => path.toAbsolutePath.normalize.startsWith(item.toAbsolutePath.normalize))
          }.flatMap(allowed => if allowed then Right(()) else Left(AppError.InvalidInput("Служебные файлы и ключи не включаются в резервную копию")))
          before <- Boundary.attempt("Свойства исходного файла")((Files.size(path), Files.getLastModifiedTime(path).toMillis))
          bytes <- Boundary.attempt("Чтение исходного файла")(Files.readAllBytes(path))
          digest <- Crypto.hash(cfg.hashAlgorithm, bytes)
          after <- Boundary.attempt("Свойства исходного файла")((Files.size(path), Files.getLastModifiedTime(path).toMillis))
          _ <- if before == after && after == (file.size, file.mtime) && digest == file.hash then Right(())
               else Left(AppError.FileChanged(path.toString))
        yield (file, bytes)
      }
      val saved = attempted.flatMap(_.toOption)
      val deletionChecks = plan.report.deleted.map { record =>
        for
          source <- record.srcIndex.flatMap(meta.sources.lift).toRight(AppError.FileChanged(record.rel))
          _ <- Either.cond(source.available, (), AppError.SourceMissing(source.path))
          _ <- Scanner.resolveRel(meta, record.rel).filter(path => Files.notExists(path) || Files.isDirectory(path))
            .toRight(AppError.FileChanged(record.rel))
        yield record
      }
      val deleted = deletionChecks.flatMap(_.toOption)
      val problems = plan.problems ++ attempted.flatMap(_.left.toOption) ++ deletionChecks.flatMap(_.left.toOption)
      val snapshot = meta.lastVersion.map(_.snapshot).getOrElse(Map.empty) ++
        saved.map { case (file, _) => file.rel -> FileRef(Some(file.hash), file.size) } ++
        deleted.map(record => record.rel -> FileRef(None, 0L))
      val newPaths = plan.report.newFiles.map(_.rel).toSet
      val (added, modified) = saved.map(_._1.rel).partition(newPaths)
      val timestamp = now()
      val version = VersionInfo(meta.versions.map(_.number).maxOption.getOrElse(0) + 1, timestamp,
        comment, if problems.isEmpty then "success" else "partial", added, modified, deleted.map(_.rel),
        plan.report.renamed.filter { case (old, fresh) => deleted.exists(_.rel == old.rel) && added.contains(fresh.rel) }
          .map { case (old, fresh) => old.rel -> fresh.rel }, saved.map(_._1.size).sum, snapshot)
      val failedFiles = files.zip(attempted).collect { case (file, Left(_)) => file.rel } ++
        plan.report.deleted.zip(deletionChecks).collect { case (record, Left(_)) => record.rel }
      val failedSources = failedFiles.flatMap(Scanner.sourceIndexOf).toSet
      val sources = meta.sources.zipWithIndex.map { case (source, index) =>
        if plan.scannedSources(index) && !failedSources(index) && source.available then source.copy(lastBackup = Some(timestamp))
        else source
      }
      val updated = retain(meta.copy(versions = meta.versions :+ version, sources = sources), cfg.maximumVersionsPerFile)
      val blobs = saved.map { case (file, bytes) => file.hash -> bytes }.toMap
      if Files.notExists(meta.containerPath) then Left(AppError.FileMissing(meta.containerPath.toString))
      else writeZip(updated, blobs, actualKey).map { _ =>
        (updated, BackupResult(version, added.size, modified.size, deleted.size, problems.size, version.bytes, problems))
      }
    }

  // Проверка целостности предшествует замене файла назначения.
  private def plainBlob(zip: ZipFile, hash: String, key: Option[String]): Either[AppError, Array[Byte]] =
    for
      stored <- entryBytes(zip, BlobPrefix + hash)
      bytes <- key.fold[Either[AppError, Array[Byte]]](Right(stored))(Crypto.decrypt(_, stored))
      actual <- Crypto.hash(if hash.length == 32 then "MD5" else "SHA-256", bytes)
      _ <- Either.cond(actual == hash, (), AppError.DamagedStorage(s"Повреждён блок $hash"))
    yield bytes

  private def versionOf(meta: StorageMeta, number: Int): Either[AppError, VersionInfo] =
    meta.versions.find(_.number == number).toRight(AppError.VersionNotFound(number))

  // Проверка границ каталога назначения и символьных ссылок.
  private def safeTarget(dest: Path, rel: String): Either[AppError, Path] =
    Boundary.attempt("Проверка пути восстановления") {
      val root = dest.toAbsolutePath.normalize
      val target = root.resolve(rel).normalize
      var parent = Option(target)
      var symlink = false
      while parent.exists(_.startsWith(root)) && !symlink do
        symlink = parent.exists(Files.isSymbolicLink(_))
        parent = parent.flatMap(path => Option(path.getParent))
      (root, target, symlink)
    }.flatMap { case (root, target, symlink) =>
      if Scanner.sourceIndexOf(rel).isEmpty || !target.startsWith(root) || target == root || symlink then
        Left(AppError.DamagedStorage(s"Небезопасный путь восстановления: $rel"))
      else Right(target)
    }

  def restoreConflicts(meta: StorageMeta, number: Int, dest: Path): Either[AppError, List[String]] =
    versionOf(meta, number).flatMap { version =>
      version.snapshot.toList.filter(_._2.hash.nonEmpty).sortBy(_._1)
        .foldLeft[Either[AppError, List[String]]](Right(Nil)) { case (result, (rel, _)) =>
          result.flatMap(all => safeTarget(dest, rel).map(path => if Files.exists(path) then all :+ rel else all))
        }
    }

  def restore(meta: StorageMeta, key: Option[String], versionNumber: Int,
              destDir: Path, overwrite: Set[String] = Set.empty,
              skipExisting: Boolean = true): Either[AppError, RestoreResult] =
    for
      actualKey <- keyFor(meta, key)
      version <- versionOf(meta, versionNumber)
      _ <- if Files.isDirectory(destDir) && Files.isWritable(destDir) then Right(())
           else Left(AppError.Io("Восстановление", s"Каталог недоступен: $destDir"))
      result <- withZip(meta.containerPath) { zip =>
        val results = for
          (rel, ref) <- version.snapshot.toList.sortBy(_._1)
          hash <- ref.hash.toList
        yield
          for
            target <- safeTarget(destDir, rel)
            allowed = !skipExisting || overwrite(rel)
            count <- if Files.exists(target) && !allowed then Right(false)
              else plainBlob(zip, hash, actualKey).flatMap { bytes =>
                Boundary.attempt("Восстановление файла") {
                  Files.createDirectories(target.getParent)
                  replaceFile(target, allowed)(temp => Files.write(temp, bytes))
                  true
                }
              }
          yield count
        Right(RestoreResult(results.count(_ == Right(true)), results.count(_ != Right(true)), results.flatMap(_.left.toOption)))
      }
    yield result

  def verify(meta: StorageMeta, key: Option[String]): Either[AppError, VerifyResult] =
    keyFor(meta, key).flatMap { actualKey => withZip(meta.containerPath) { zip =>
      val hashes = meta.versions.flatMap(_.snapshot.values.flatMap(_.hash)).distinct
      val missing = hashes.filter(hash => Option(zip.getEntry(BlobPrefix + hash)).isEmpty)
      val checked = hashes.filterNot(missing.contains).map(hash => hash -> plainBlob(zip, hash, actualKey))
      Right(VerifyResult(checked.count(_._2.isRight), checked.collect { case (hash, Left(_)) => hash }, missing))
    }}
