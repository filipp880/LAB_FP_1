package backup

import java.nio.file.{Files, Paths}
import scala.util.control.NonFatal

// Индексы источников сохраняются после удаления для согласованности со снимками.
case class Source(
    path: String,
    includeSubdirs: Boolean,
    exclusions: List[String],
    enabled: Boolean = true,
    lastBackup: Option[String] = None,
    removed: Boolean = false
):
  def available: Boolean =
    try
      val source = Paths.get(path)
      Files.exists(source) && Files.isReadable(source)
    catch case NonFatal(_) => false

  def status: String = if available then "доступен" else "недоступен"

object Source:
  def toJson(source: Source): ujson.Value =
    val json = ujson.Obj()
    json("path") = source.path
    json("includeSubdirs") = source.includeSubdirs
    json("exclusions") = ujson.Arr.from(source.exclusions)
    json("enabled") = source.enabled
    json("lastBackup") = ModelJson.optionalText(source.lastBackup)
    json("removed") = source.removed
    json

  def fromJson(value: ujson.Value): Source =
    val exclusionValues = value("exclusions").arr.toList
    val exclusions = exclusionValues.map(item => item.str)
    val removed = value("removed").bool
    Source(
      value("path").str, value("includeSubdirs").bool,
      exclusions, value("enabled").bool,
      ModelJson.readOptionalText(value("lastBackup")),
      removed
    )

  def validate(source: Source): Either[AppError, Source] =
    if source.path.trim.isEmpty then Left(AppError.InvalidInput("Путь источника не указан"))
    else
      val invalidPattern = source.exclusions.find(pattern => !Glob.isValidPattern(pattern))
      invalidPattern match
        case Some(pattern) => Left(AppError.InvalidPattern(pattern))
        case None =>
          try
            val path = Paths.get(source.path).toAbsolutePath.normalize()
            if !Files.exists(path) then Left(AppError.SourceMissing(path.toString))
            else Right(source.copy(path = path.toString))
          catch case NonFatal(_) => Left(AppError.InvalidPath("Не удалось проверить путь источника"))
