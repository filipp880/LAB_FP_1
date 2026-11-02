package backup

import java.nio.file.{AccessDeniedException, FileAlreadyExistsException, NoSuchFileException}
import scala.util.control.NonFatal

enum AppError(val message: String):
  case UnknownCommand extends AppError("Такого пункта или команды нет. Введите номер из главного меню, например 1.")
  case InvalidInput(reason: String) extends AppError(s"Некорректный ввод: $reason")
  case NoStorage extends AppError("Сначала создайте или откройте хранилище.")
  case FileExists(path: String) extends AppError(s"Файл уже существует: $path. Выберите другое имя.")
  case FileMissing(path: String) extends AppError(s"Файл отсутствует: $path")
  case InvalidConfig(reason: String) extends AppError(s"Ошибка конфигурации: $reason")
  case DamagedStorage(reason: String) extends AppError(s"Хранилище повреждено или имеет неподдерживаемый формат: $reason")
  case KeyRequired extends AppError("Требуется ключ доступа.")
  case WrongKey extends AppError("Неправильный ключ доступа.")
  case SourceNotFound(number: Int) extends AppError(s"Источник $number не найден.")
  case SourceMissing(path: String) extends AppError(s"Источник недоступен: $path")
  case VersionNotFound(number: Int) extends AppError(s"Версия $number не найдена.")
  case NoVersions extends AppError("Сохранённых версий пока нет.")
  case FileChanged(path: String) extends AppError(s"Файл изменился во время копирования и пропущен: $path")
  case UnsupportedHash(name: String) extends AppError(s"Неподдерживаемый алгоритм хеша: $name")
  case InvalidPattern(pattern: String) extends AppError(s"Некорректное исключение: $pattern")
  case Io(operation: String, detail: String) extends AppError(s"$operation: $detail")

object Boundary:

  // Исключения внешнего API преобразуются в типизированные ошибки приложения.
  def attempt[A](operation: String)(body: => A): Either[AppError, A] =
    try Right(body)
    catch
      case e: NoSuchFileException => Left(AppError.FileMissing(e.getFile))
      case e: FileAlreadyExistsException => Left(AppError.FileExists(e.getFile))
      case e: AccessDeniedException => Left(AppError.Io(operation, s"Нет доступа: ${e.getFile}"))
      case NonFatal(e) => Left(AppError.Io(operation, Option(e.getMessage).getOrElse(e.getClass.getSimpleName)))
