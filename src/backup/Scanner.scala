package backup

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object Scanner:
  private case class SourceScan(index: Int, files: List[WalkedFile], problems: List[AppError])

  // Внутри контейнера пути начинаются с номера источника: s0/photo.jpg.
  def sourceIndexOf(rel: String): Option[Int] =
    val parts = rel.split("/", 2)
    if parts.length == 2 && parts(0).startsWith("s") then
      val digits = parts(0).drop(1)
      val number = digits.toIntOption
      number.filter(value => value >= 0)
    else None

  def resolveRel(meta: StorageMeta, rel: String): Option[Path] =
    val source = sourceIndexOf(rel).flatMap(index => meta.sources.lift(index))
    source.flatMap { item =>
      try
        val base = Paths.get(item.path)
        val within = rel.split("/", 2)(1)
        val isSourceFile = Files.isRegularFile(base) && within == base.getFileName.toString
        if isSourceFile then Some(base)
        else Some(base.resolve(within))
      catch case NonFatal(_) => None
    }

  private def excluded(source: Source, rel: String, defaults: List[String]): Boolean =
    val patterns = source.exclusions ++ defaults
    val names = rel.split("/")
    patterns.exists { pattern =>
      val matchesPath = Glob.matches(pattern, rel)
      val matchesName = names.exists(name => Glob.matches(pattern, name))
      matchesPath || matchesName
    }

  private def readDirectory(path: Path): Either[AppError, List[Path]] =
    try
      val stream = Files.newDirectoryStream(path)
      try
        val entries = stream.iterator.asScala.toList
        Right(entries.sortBy(entry => entry.getFileName.toString))
      finally stream.close()
    catch case NonFatal(_) => Left(AppError.FileListFailed(path.toString))

  private def inspectFile(path: Path, rel: String, algo: String): Either[AppError, WalkedFile] =
    try
      val size = Files.size(path)
      val modified = Files.getLastModifiedTime(path).toMillis
      Crypto.hash(algo, path).flatMap { hash =>
        val sameSize = size == Files.size(path)
        val sameTime = modified == Files.getLastModifiedTime(path).toMillis
        if sameSize && sameTime then Right(WalkedFile(rel, size, modified, hash))
        else Left(AppError.FileChanged(path.toString))
      }
    catch case NonFatal(_) => Left(AppError.ReadFailed(path.toString))

  private def walk(index: Int, source: Source, algo: String, defaults: List[String],
                   protectedPaths: List[Path]): SourceScan =
    val files = ArrayBuffer.empty[WalkedFile]
    val problems = ArrayBuffer.empty[AppError]
    val base = Paths.get(source.path)
    val firstName = if Files.isDirectory(base) then "" else base.getFileName.toString
    val pending = ArrayBuffer.empty[(Path, String)]
    pending += ((base, firstName))
    var nextIndex = 0
    // Добавляем найденные пути в конец списка и проверяем их по очереди.
    while nextIndex < pending.length do
      val (path, within) = pending(nextIndex)
      nextIndex += 1
      val absolutePath = path.toAbsolutePath.normalize()
      val protectedFile = protectedPaths.exists(root => absolutePath.startsWith(root))
      // Символьная ссылка ведёт на другой путь. Не обходим её, чтобы не зациклиться.
      if !Files.isSymbolicLink(path) && !protectedFile && !excluded(source, within, defaults) then
        if Files.isDirectory(path) then
          if path == base || source.includeSubdirs then
            readDirectory(path) match
              case Left(error) => problems += error
              case Right(entries) =>
                for child <- entries do
                  val name = child.getFileName.toString
                  val childPath = if within.isEmpty then name else within + "/" + name
                  pending += ((child, childPath))
        else
          inspectFile(path, s"s$index/$within", algo) match
            case Right(file) => files += file
            case Left(error) => problems += error
    SourceScan(index, files.toList, problems.toList)

  private def wasDeleted(record: LastRecord, meta: StorageMeta, scanned: List[Int],
                         defaults: List[String]): Boolean =
    val sourceWasScanned = record.srcIndex.exists(index => scanned.contains(index))
    val source = record.srcIndex.flatMap(index => meta.sources.lift(index))
    val fileWasIncluded = source.exists { value =>
      val within = record.rel.split("/", 2)(1)
      val included = value.includeSubdirs || !within.contains('/')
      included && !excluded(value, within, defaults)
    }
    val fileIsAbsent = resolveRel(meta, record.rel).exists { path =>
      Files.notExists(path) || Files.isDirectory(path)
    }
    sourceWasScanned && fileWasIncluded && fileIsAbsent

  def scan(meta: StorageMeta, algo: String, defaults: List[String],
           protectedPaths: List[Path] = Nil): Either[AppError, ScanPlan] =
    Crypto.normalize(algo).flatMap { _ =>
      try
        val protectedFiles = meta.containerPath :: protectedPaths
        val protectedAbsolute = protectedFiles.map(path => path.toAbsolutePath.normalize)
        val walked = ArrayBuffer.empty[SourceScan]
        val missingPaths = ArrayBuffer.empty[String]
        for index <- meta.sources.indices do
          val source = meta.sources(index)
          if source.enabled && !source.removed then
            if source.available then walked += walk(index, source, algo, defaults, protectedAbsolute)
            else missingPaths += source.path

        val results = walked.toList
        val currentFiles = results.flatMap(result => result.files)
        val current = currentFiles.sortBy(file => file.rel)
        val last = meta.lastVersion.map(_.snapshot).getOrElse(Map.empty)
        val previousEntries = last.toList
        val previous = previousEntries.flatMap { case (rel, ref) =>
          ref.hash.map(hash => LastRecord(rel, hash, ref.size, sourceIndexOf(rel)))
        }
        val newFiles = current.filter { file =>
          val existedBefore = previous.exists(record => record.rel == file.rel)
          !existedBefore
        }
        val changed = ArrayBuffer.empty[(LastRecord, WalkedFile)]
        for file <- current do
          previous.find(record => record.rel == file.rel) match
            case Some(old) =>
              if old.hash != file.hash then changed += ((old, file))
            case None => ()

        // Сбой чтения или отключённый источник не означает, что файлы удалили.
        var scanned = List.empty[Int]
        for result <- walked do
          if result.problems.isEmpty then scanned = scanned :+ result.index
        val absentFiles = previous.filter { record =>
          val existsNow = current.exists(file => file.rel == record.rel)
          !existsNow && wasDeleted(record, meta, scanned, defaults)
        }
        val deleted = absentFiles.sortBy(record => record.rel)

        // Считаем файл переименованным, если совпали содержимое и размер.
        val renamed = ArrayBuffer.empty[(LastRecord, WalkedFile)]
        var matchedFiles = List.empty[String]
        for old <- deleted do
          val replacement = newFiles.find { file =>
            file.hash == old.hash && file.size == old.size && !matchedFiles.contains(file.rel)
          }
          replacement match
            case Some(file) =>
              renamed += ((old, file))
              matchedFiles = matchedFiles :+ file.rel
            case None => ()

        val errors = results.flatMap(result => result.problems)
        val unavailable = missingPaths.toList
        val missing = unavailable.map(path => AppError.SourceMissing(path))
        val messages = errors.map(error => error.message)
        val report = ChangeReport(newFiles, changed.toList, deleted, renamed.toList, messages,
          unavailable, current.size)
        Right(ScanPlan(report, errors ++ missing, scanned))
      catch case NonFatal(_) => Left(AppError.ScanFailed)
    }
