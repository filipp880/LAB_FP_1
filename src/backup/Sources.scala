package backup

import java.nio.file.{Files, Paths}
import upickle.default.*
import ModelJson.given

// Удалённые источники остаются в списке, чтобы не сдвигать номера в старых снимках.
final case class Source(
    path: String,
    includeSubdirs: Boolean,
    exclusions: List[String],
    enabled: Boolean = true,
    lastBackup: Option[String] = None,
    removed: Boolean = false
) derives ReadWriter:
  def available: Boolean = Boundary.attempt("Проверка источника") {
    val source = Paths.get(path)
    Files.exists(source) && Files.isReadable(source)
  }.getOrElse(false)

  def status: String = if available then "доступен" else "недоступен"

object Source:
  def validate(source: Source): Either[AppError, Source] =
    if source.path.trim.isEmpty then Left(AppError.InvalidInput("Путь источника не указан"))
    else source.exclusions.find(pattern => !Glob.isValidPattern(pattern)) match
      case Some(pattern) => Left(AppError.InvalidPattern(pattern))
      case None => Boundary.attempt("Проверка пути источника") {
        Paths.get(source.path).toAbsolutePath.normalize()
      }.flatMap { path =>
        if !Files.exists(path) then Left(AppError.SourceMissing(path.toString))
        else Right(source.copy(path = path.toString))
      }
