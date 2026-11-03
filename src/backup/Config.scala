package backup

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, NoSuchFileException, Path, Paths}
import scala.util.control.NonFatal

case class Config(
    logFile: String,
    keyDirectory: String,
    defaultContainerName: String,
    checkOnStorageOpen: Boolean,
    confirmBeforeBackup: Boolean,
    confirmBeforeRestoreOverwrite: Boolean,
    maximumVersionsPerFile: Int,
    hashAlgorithm: String,
    defaultExclusions: List[String]
)

object Config:
  val Default = Config("backup.log", "keys", "backup.store", true, true, true,
    10, "SHA-256", List("*.tmp", "*.log", "cache", "node_modules"))
  val defaultPath: Path = Paths.get("backup.json")

  // str читает строку JSON, bool — логическое значение, num — число.
  private def parse(text: String): Either[AppError, Config] =
    try
      val json = ujson.read(text)
      val maximum = json("maximumVersionsPerFile").num
      val wholeNumber = maximum.toInt
      val exclusionValues = json("defaultExclusions").arr.toList
      val exclusions = exclusionValues.map(value => value.str)
      if maximum != wholeNumber.toDouble then
        Left(AppError.InvalidConfig("Число версий должно быть целым Int"))
      else
        Right(Config(
          json("logFile").str,
          json("keyDirectory").str,
          json("defaultContainerName").str,
          json("checkOnStorageOpen").bool,
          json("confirmBeforeBackup").bool,
          json("confirmBeforeRestoreOverwrite").bool,
          wholeNumber,
          json("hashAlgorithm").str,
          exclusions
        ))
    catch case NonFatal(_) => Left(AppError.InvalidConfig("Неправильный формат JSON или тип поля"))

  def toJson(config: Config): ujson.Value =
    val json = ujson.Obj()
    json("logFile") = config.logFile
    json("keyDirectory") = config.keyDirectory
    json("defaultContainerName") = config.defaultContainerName
    json("checkOnStorageOpen") = config.checkOnStorageOpen
    json("confirmBeforeBackup") = config.confirmBeforeBackup
    json("confirmBeforeRestoreOverwrite") = config.confirmBeforeRestoreOverwrite
    json("maximumVersionsPerFile") = config.maximumVersionsPerFile
    json("hashAlgorithm") = config.hashAlgorithm
    json("defaultExclusions") = ujson.Arr.from(config.defaultExclusions)
    json

  def load(path: Path): Either[AppError, Config] =
    try
      parse(Files.readString(path, UTF_8)).flatMap { config =>
        validate(config).map(_ => config)
      }
    catch
      case _: NoSuchFileException => Left(AppError.FileMissing(path.toString))
      case NonFatal(_) => Left(AppError.ReadFailed(path.toString))

  def validate(config: Config): Either[AppError, Unit] =
    if config.logFile.trim.isEmpty then Left(AppError.InvalidConfig("Путь к журналу не указан"))
    else if config.keyDirectory.trim.isEmpty then Left(AppError.InvalidConfig("Каталог для ключей не указан"))
    else if config.defaultContainerName.trim.isEmpty then Left(AppError.InvalidConfig("Имя контейнера не указано"))
    else if config.maximumVersionsPerFile < 1 then Left(AppError.InvalidConfig("Максимальное количество версий меньше единицы"))
    else
      Crypto.normalize(config.hashAlgorithm).flatMap { _ =>
        config.defaultExclusions.find(pattern => !Glob.isValidPattern(pattern)) match
          case Some(pattern) => Left(AppError.InvalidPattern(pattern))
          case None => validatePaths(config)
      }

  private def validatePaths(config: Config): Either[AppError, Unit] =
    try
      val log = Paths.get(config.logFile)
      val keys = Paths.get(config.keyDirectory)
      val name = config.defaultContainerName
      val hasDirectory = name.contains("/") || name.contains("\\")
      if hasDirectory || name == "." || name == ".." then
        Left(AppError.InvalidConfig("Имя контейнера должно быть именем файла без каталога"))
      else if Files.exists(keys) && !Files.isDirectory(keys) then
        Left(AppError.InvalidConfig("По пути каталога ключей находится файл"))
      else if Files.isDirectory(log) then
        Left(AppError.InvalidConfig("Вместо файла журнала указан каталог"))
      else Right(())
    catch case NonFatal(_) => Left(AppError.InvalidConfig("Некорректный путь в настройках"))

  def save(path: Path, config: Config): Either[AppError, Unit] =
    validate(config).flatMap { _ =>
      try
        Files.writeString(path, ujson.write(toJson(config), indent = 2), UTF_8)
        Right(())
      catch case NonFatal(_) => Left(AppError.WriteFailed(path.toString))
    }
