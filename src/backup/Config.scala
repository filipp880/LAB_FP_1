package backup

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.util.control.NonFatal
import upickle.default.*

final case class Config(
    logFile: String,
    keyDirectory: String,
    defaultContainerName: String,
    checkOnStorageOpen: Boolean,
    confirmBeforeBackup: Boolean,
    confirmBeforeRestoreOverwrite: Boolean,
    maximumVersionsPerFile: Int,
    hashAlgorithm: String,
    defaultExclusions: List[String]
) derives ReadWriter

object Config:
  val Default = Config("backup.log", "keys", "backup.store", true, true, true,
    10, "SHA-256", List("*.tmp", "*.log", "cache", "node_modules"))
  val ConfigFileName = "backup.json"
  def defaultPath: Path = Paths.get(ConfigFileName).toAbsolutePath

  // Строгая проверка типов полей JSON без неявных преобразований.
  private def parse(text: String): Either[AppError, Config] =
    try

      val value = ujson.read(text)
      val maximum = value("maximumVersionsPerFile").num
      if maximum != maximum.toInt.toDouble then Left(AppError.InvalidConfig("Число версий должно быть целым Int"))
      else Right(Config(
        value("logFile").str, value("keyDirectory").str,
        value("defaultContainerName").str, value("checkOnStorageOpen").bool,
        value("confirmBeforeBackup").bool, value("confirmBeforeRestoreOverwrite").bool,
        maximum.toInt, value("hashAlgorithm").str,
        value("defaultExclusions").arr.map(_.str).toList
      ))
    catch case NonFatal(_) => Left(AppError.InvalidConfig("Неправильный формат JSON или тип поля"))

  def toJson(config: Config): ujson.Value = writeJs(config)

  // Отсутствие конфигурационного файла при reload является ошибкой.
  def load(path: Path): Either[AppError, Config] =
    val loaded = Boundary.attempt("Чтение конфигурации")(Files.readString(path, UTF_8)).flatMap(parse)
    loaded.flatMap(config => validate(config).map(_ => config))

  // Проверка ограничений конфигурации и доступности служебных путей.
  def validate(config: Config): Either[AppError, Unit] =
    if config.logFile.trim.isEmpty then Left(AppError.InvalidConfig("Путь к журналу не указан"))
    else if config.keyDirectory.trim.isEmpty then Left(AppError.InvalidConfig("Каталог для ключей не указан"))
    else if config.defaultContainerName.trim.isEmpty then Left(AppError.InvalidConfig("Имя контейнера не указано"))
    else if config.maximumVersionsPerFile < 1 then Left(AppError.InvalidConfig("Максимальное количество версий меньше единицы"))
    else Crypto.normalize(config.hashAlgorithm).flatMap { _ =>
      config.defaultExclusions.find(pattern => !Glob.isValidPattern(pattern)) match
        case Some(pattern) => Left(AppError.InvalidPattern(pattern))
        case None =>
          try
            val log = Paths.get(config.logFile).toAbsolutePath
            val keys = Paths.get(config.keyDirectory).toAbsolutePath
            val name = Paths.get(config.defaultContainerName)
            if name.isAbsolute || name.getNameCount != 1 || Set(".", "..").contains(config.defaultContainerName) then
              Left(AppError.InvalidConfig("Имя контейнера должно быть именем файла без каталога"))
            else
              Files.createDirectories(keys)
              if !Files.isWritable(keys) then Left(AppError.InvalidConfig("Каталог ключей недоступен для записи"))
              else if Files.exists(log) && (!Files.isRegularFile(log) || !Files.isWritable(log)) then
                Left(AppError.InvalidConfig("Журнал недоступен для записи"))
              else Right(())
          catch case NonFatal(_) => Left(AppError.InvalidConfig("Некорректный путь или невозможно создать каталог ключей"))
    }

  def save(path: Path, config: Config): Either[AppError, Unit] =
    validate(config).flatMap(_ => Boundary.attempt("Запись конфигурации") {
      Files.writeString(path, write(config, indent = 2), UTF_8)
      ()
    })
