package backup

import java.nio.file.{Files, LinkOption, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

object Scanner:
  def sourceIndexOf(rel: String): Option[Int] =
    rel.split("/", 2).toList match
      case prefix :: _ :: Nil if prefix.startsWith("s") =>
        prefix.drop(1).toIntOption.filter(_ >= 0)
      case _ => None

  // Переводит внутренний путь sN/имя в исходный путь на диске.
  def resolveRel(meta: StorageMeta, rel: String): Option[Path] =
    for
      index <- sourceIndexOf(rel)
      source <- meta.sources.lift(index)
      within <- rel.split("/", 2).lift(1)
      path <- Boundary.attempt("Разбор пути") {
        val base = Paths.get(source.path)
        if Files.isRegularFile(base) && within == base.getFileName.toString then base else base.resolve(within)
      }.toOption
    yield path

  private def excluded(source: Source, rel: String, defaults: List[String]): Boolean =
    (source.exclusions ++ defaults).exists { pattern =>
      Glob.matches(pattern, rel) || rel.split("/").exists(Glob.matches(pattern, _))
    }

  private def walk(index: Int, source: Source, algo: String, defaults: List[String],
                   protectedPaths: List[Path]): (List[WalkedFile], List[AppError]) =
    val files = List.newBuilder[WalkedFile]
    val problems = List.newBuilder[AppError]
    val base = Paths.get(source.path)

    def visit(path: Path, within: String): Unit =
      if !Files.isSymbolicLink(path) && !protectedPaths.exists(path.toAbsolutePath.normalize.startsWith(_)) &&
          !excluded(source, within, defaults) then
        if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then
          if path == base || source.includeSubdirs then
            Boundary.attempt("Чтение каталога") {
              Using.resource(Files.newDirectoryStream(path))(_.iterator.asScala.toList.sortBy(_.getFileName.toString))
            } match
              case Left(error) => problems += error
              case Right(entries) => entries.foreach { child =>
                val name = child.getFileName.toString
                visit(child, if within.isEmpty then name else s"$within/$name")
              }
        else
          val result = for
            before <- Boundary.attempt("Свойства файла")((Files.size(path), Files.getLastModifiedTime(path).toMillis))
            digest <- Crypto.hash(algo, path)
            after <- Boundary.attempt("Свойства файла")((Files.size(path), Files.getLastModifiedTime(path).toMillis))
            file <- if before == after then Right(WalkedFile(s"s$index/$within", before._1, before._2, digest))
                    else Left(AppError.FileChanged(path.toString))
          yield file
          result match
            case Right(file) => files += file
            case Left(error) => problems += error

    val within = if Files.isDirectory(base) then "" else base.getFileName.toString
    visit(base, within)
    (files.result(), problems.result())

  def scan(meta: StorageMeta, algo: String, defaults: List[String],
           protectedPaths: List[Path] = Nil): Either[AppError, ScanPlan] =
    Crypto.normalize(algo).flatMap { _ => Boundary.attempt("Сканирование") {
      val protectedAbsolute = (meta.containerPath :: protectedPaths).map(_.toAbsolutePath.normalize)
      val active = meta.sources.zipWithIndex.filter { case (source, _) => source.enabled && !source.removed }
      val (available, unavailable) = active.partition(_._1.available)
      val walked = available.map { case (source, index) =>
        val (files, errors) = walk(index, source, algo, defaults, protectedAbsolute)
        (index, files, errors)
      }
      val current = walked.flatMap(_._2).map(file => file.rel -> file).toMap
      val last = meta.lastVersion.map(_.snapshot).getOrElse(Map.empty)
      val previous = last.toList.flatMap { case (rel, ref) =>
        ref.hash.map(hash => LastRecord(rel, hash, ref.size, sourceIndexOf(rel)))
      }
      val newFiles = current.values.filter(file => last.get(file.rel).flatMap(_.hash).isEmpty).toList.sortBy(_.rel)
      val changed = previous.flatMap(record => current.get(record.rel)
        .filter(_.hash != record.hash).map(record -> _)).sortBy(_._1.rel)
      val safelyScanned = walked.filter(_._3.isEmpty).map(_._1).toSet
      // Удаление подтверждается только для успешно проверенного источника.
      val deleted = previous.filter { record =>
        !current.contains(record.rel) && record.srcIndex.exists { index =>
          safelyScanned(index) && meta.sources.lift(index).exists { source =>
            val within = record.rel.split("/", 2).last
            (source.includeSubdirs || !within.contains('/')) && !excluded(source, within, defaults)
          }
        } && resolveRel(meta, record.rel).exists(path => Files.notExists(path) || Files.isDirectory(path))
      }.sortBy(_.rel)
      val oldByHash = deleted.groupBy(record => (record.hash, record.size))
      val newByHash = newFiles.groupBy(file => (file.hash, file.size))
      // Совпадение хеша и размера — предположение о переименовании.
      val renamed = oldByHash.keySet.intersect(newByHash.keySet).toList.sorted.flatMap { key =>
        oldByHash(key).zip(newByHash(key))
      }
      val errors = walked.flatMap(_._3)
      val missing = unavailable.map { case (source, _) => AppError.SourceMissing(source.path) }
      ScanPlan(ChangeReport(newFiles, changed, deleted, renamed, errors.map(_.message),
        unavailable.map(_._1.path), current.size), errors ++ missing, safelyScanned)
    }}
