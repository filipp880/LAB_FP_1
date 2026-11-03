package backup

enum SourceAction:
  case Menu, ListAll, Add
  case Edit(number: Int)
  case Remove(number: Int)
  case SetEnabled(number: Int, enabled: Boolean)

enum Command:
  case Help, Create, Open, Info, Scan, Versions, Verify, Reload, Exit
  case Sources(action: SourceAction)
  case Backup(comment: Option[String])
  case Restore(version: Option[Int])

object Command:
  def positiveInt(text: String): Option[Int] =
    text.toIntOption match
      case Some(number) if number > 0 => Some(number)
      case _ => None

  // Синтаксический разбор ввода с формированием типизированной команды.
  def parse(line: String): Either[AppError, Command] =
    def number(text: String): Either[AppError, Int] =
      positiveInt(text) match
        case Some(value) => Right(value)
        case None => Left(AppError.InvalidInput("Нужен положительный целый номер"))
    line.trim.split("\\s+").toList match
      case ("11" | "help" | "") :: Nil => Right(Help)
      case ("1" | "create") :: Nil => Right(Create)
      case ("2" | "open") :: Nil => Right(Open)
      case ("3" | "info") :: Nil => Right(Info)
      case ("5" | "scan") :: Nil => Right(Scan)
      case ("7" | "versions") :: Nil => Right(Versions)
      case ("9" | "verify") :: Nil => Right(Verify)
      case ("10" | "reload") :: Nil => Right(Reload)
      case "exit" :: Nil | "0" :: Nil => Right(Exit)
      case "6" :: Nil => Right(Backup(None))
      case "backup" :: Nil => Right(Backup(None))
      case "backup" :: words => Right(Backup(Some(words.mkString(" "))))
      case ("8" | "restore") :: Nil => Right(Restore(None))
      case "restore" :: n :: Nil => number(n).map(n => Restore(Some(n)))
      case ("4" | "sources") :: Nil => Right(Sources(SourceAction.Menu))
      case "sources" :: "list" :: Nil => Right(Sources(SourceAction.ListAll))
      case "sources" :: "add" :: Nil => Right(Sources(SourceAction.Add))
      case "sources" :: "edit" :: n :: Nil => number(n).map(n => Sources(SourceAction.Edit(n)))
      case "sources" :: "remove" :: n :: Nil => number(n).map(n => Sources(SourceAction.Remove(n)))
      case "sources" :: "enable" :: n :: Nil => number(n).map(n => Sources(SourceAction.SetEnabled(n, true)))
      case "sources" :: "disable" :: n :: Nil => number(n).map(n => Sources(SourceAction.SetEnabled(n, false)))
      case _ => Left(AppError.UnknownCommand)
