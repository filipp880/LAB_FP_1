package backup

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters.*

final class Menu(initialConfig: Config):
  private var cfg = initialConfig
  private var log = new Logger(cfg.logFile)
  private var opened: Option[StorageMeta] = None
  private var key: Option[String] = None
  private var running = true

  // Для терминала используется его кодировка, для IDEA и перенаправленного ввода — UTF-8.
  private val input = new java.io.BufferedReader(Option(System.console()).map(_.reader()).getOrElse(
    new java.io.InputStreamReader(System.in, java.nio.charset.StandardCharsets.UTF_8)))

  private def event(message: String): Unit =
    log.info(message).left.foreach(e => Console.err.println(e.message))

  private def error(e: AppError): Unit =
    println(e.message)
    logError(e)

  // В журнал попадает только категория ошибки, без ключей и полного ввода пользователя.
  private def logError(e: AppError): Unit =

    val category = e match
      case AppError.Io(operation, _) => s"Io: $operation"
      case _ => e.productPrefix
    event(s"Ошибка: $category")

  // Показывает ошибку и прерывает текущую цепочку диалога через None.
  private def show[A](result: Either[AppError, A]): Option[A] =
    result.fold(e => { error(e); None }, Some(_))

  // None означает конец ввода; пустая строка остаётся отдельным ответом.
  private def read(prompt: String): Option[String] =
    print(prompt)
    val value = Option(input.readLine()).map(_.trim)
    if value.isEmpty then running = false
    value

  private def path(text: String): Option[Path] =
    show(Boundary.attempt("Разбор пути")(Paths.get(text).toAbsolutePath.normalize))

  @scala.annotation.tailrec
  private def yes(question: String, default: Boolean = false): Option[Boolean] =
    println(s"$question  1 — Да, 2 — Нет (Enter — ${if default then "Да" else "Нет"})")
    read("Ваш ответ: ").map(_.toLowerCase(java.util.Locale.ROOT)) match
      case None => None
      case Some("") => Some(default)
      case Some("да" | "д" | "yes" | "y" | "1") => Some(true)
      case Some("нет" | "н" | "no" | "n" | "0" | "2") => Some(false)
      case _ => println("Введите 1 (да), 2 (нет) или нажмите Enter."); yes(question, default)

  @scala.annotation.tailrec
  private def readNumber(prompt: String): Option[Int] = read(prompt) match
    case None | Some("") => None
    case Some(text) => Command.positiveInt(text) match
      case Some(number) => Some(number)
      case None => println("Введите целый номер, например 1. Enter — отмена."); readNumber(prompt)

  private def withStorage(action: StorageMeta => Unit): Unit = opened match
    case Some(meta) => action(meta)
    case None => error(AppError.NoStorage)

  private def info(meta: StorageMeta): Unit =
    println(s"Хранилище: ${meta.name}\nФайл: ${meta.containerPath}")
    println(s"Шифрование: ${if meta.encrypted then "включено" else "выключено"}")
    println(s"Источников: ${meta.sources.count(s => !s.removed)}")
    println(s"Последняя копия: ${meta.lastVersion.map(_.timestamp).getOrElse("нет")}")
    println(s"Сохранённых версий: ${meta.versions.size}")

  private def sources(meta: StorageMeta): Unit =
    if meta.sources.forall(_.removed) then println("Источников пока нет. Добавьте файл или папку, которые нужно копировать.")
    meta.sources.zipWithIndex.filterNot(_._1.removed).foreach { (s, i) =>
      println(s"${i + 1}. ${s.path}")
      println(s"   Вложенные папки: ${if s.includeSubdirs then "да" else "нет"}; копировать: ${if s.enabled then "да" else "нет"}; ${s.status}")
      println(s"   Исключения: ${s.exclusions.mkString(", ")}; последняя копия: ${s.lastBackup.getOrElse("нет")}")
    }

  private def readSource(): Option[Source] =
    for
      text <- read("Путь к файлу или каталогу: ").filter(_.nonEmpty)
      sourcePath <- path(text)
      recursive <- yes("Копировать также вложенные папки?", true)
      exclusions <- read("Исключения через запятую (Enter — из конфигурации): ")
      patterns = if exclusions.isEmpty then cfg.defaultExclusions else exclusions.split(",").toList.map(_.trim)
      source <- show(Source.validate(Source(sourcePath.toString, recursive, patterns)))
    yield source

  private def readSources(): Option[List[Source]] =
    for
      source <- readSource()
      more <- yes("Добавить ещё файл или папку?")
      rest <- if more then readSources() else Some(Nil)
    yield source :: rest

  private def changeSources(meta: StorageMeta, action: SourceAction): Unit =
    def save(updated: List[Source]): Unit =
      val next = meta.copy(sources = updated)
      show(Container.save(next, key)).foreach { _ =>
        opened = Some(next)
        event(s"Изменение источников: $action")
        sources(next)
      }
    def update(number: Int)(f: Source => Option[Source]): Unit =
      meta.sources.lift(number - 1).filterNot(_.removed) match
        case None => error(AppError.SourceNotFound(number))
        case Some(old) => f(old).foreach(s => save(meta.sources.updated(number - 1, s)))
    action match
      case SourceAction.Menu => sourcesMenu()
      case SourceAction.ListAll => sources(meta)
      case SourceAction.Add => readSource().foreach(s => save(meta.sources :+ s))
      case SourceAction.Edit(n) => update(n) { old =>
        readSource().map(s => s.copy(enabled = old.enabled,
          lastBackup = if s.path == old.path then old.lastBackup else None))
      }

      case SourceAction.Remove(n) => update(n)(s => Some(s.copy(enabled = false, removed = true)))
      case SourceAction.SetEnabled(n, enabled) => update(n)(s => Some(s.copy(enabled = enabled)))

  private def sourcesMenu(): Unit =
    var stay = true
    while stay && running do
      withStorage { meta =>
        println("\n=== ФАЙЛЫ И ПАПКИ ДЛЯ КОПИРОВАНИЯ ===")
        sources(meta)
        println("\n1. Добавить файл или папку\n2. Изменить источник\n3. Удалить из списка\n4. Включить копирование\n5. Отключить копирование\n0. Назад в главное меню")
        read("Выберите действие (0–5): ").foreach {
          case "0" | "" => stay = false
          case "1" => changeSources(meta, SourceAction.Add)
          case choice @ ("2" | "3" | "4" | "5") =>
            readNumber("Номер источника из списка выше (Enter — отмена): ").foreach { number =>
              val action = choice match
                case "2" => SourceAction.Edit(number)
                case "3" => SourceAction.Remove(number)
                case "4" => SourceAction.SetEnabled(number, true)
                case _ => SourceAction.SetEnabled(number, false)
              changeSources(meta, action)
            }
          case _ => println("Такого пункта нет. Выберите номер от 0 до 5.")
        }
      }

  private def chooseDirectory(): Option[Path] =
    val roots = java.nio.file.FileSystems.getDefault.getRootDirectories.asScala.toList
    event("Обнаружение доступных устройств")
    println("\nГде сохранить файл хранилища? Диски:")
    roots.zipWithIndex.foreach { (root, i) =>
      val note = if Files.isWritable(root) then "" else " (выберите свою папку на этом диске)"
      println(s"${i + 1}. $root$note")
    }
    println(s"Можно ввести полный путь к папке, например ${Paths.get(System.getProperty("user.home"), "Desktop")}.")
    read("Номер диска или путь к папке (Enter — отмена): ").filter(_.nonEmpty).flatMap { text =>
      val selected = Command.positiveInt(text) match
        case Some(n) => roots.lift(n - 1)
        case None => path(text)
      selected match
        case Some(p) if Files.isDirectory(p) && Files.isWritable(p) => Some(p)
        case _ =>
          println("В эту папку нельзя записать файл. Укажите существующую папку, к которой у вас есть доступ.")
          chooseDirectory()
    }

  private def keyFile(name: String): Path =
    Paths.get(cfg.keyDirectory).resolve(name.replaceAll("[\\\\/:*?\"<>|]", "_") + ".key")

  private def newKey(name: String): Option[String] =
    val generated = Crypto.generateKey()
    println(s"Как хранить ключ?\n1. Сохранить в каталоге ${cfg.keyDirectory}\n2. Показать ключ, чтобы вы сохранили его сами")
    read("Выберите 1 или 2: ").flatMap {
      case "1" =>
        show(Boundary.attempt("Сохранение ключа") {
          val file = keyFile(name)
          Files.createDirectories(file.toAbsolutePath.getParent)
          Files.writeString(file, generated, StandardOpenOption.CREATE_NEW)
          file
        }).map { file =>
          println(s"Ключ сохранён: $file. Потеря файла ключа сделает хранилище недоступным.")
          generated
        }
      case "2" =>
        println(s"Ключ: $generated\nПрограмма его не сохранит. Без ключа открыть хранилище невозможно.")
        yes("Вы сохранили показанный ключ?").filter(identity).map(_ => generated)
      case _ => error(AppError.InvalidInput("Выберите 1 или 2")); None
    }

  private def create(): Unit =
    println("\n=== СОЗДАНИЕ ХРАНИЛИЩА ===\nХранилище — один файл с резервными копиями. Сначала выберите папку для этого файла.")
    for
      directory <- chooseDirectory()
      name <- read("Название хранилища: ").filter(_.nonEmpty)
      filename <- read(s"Имя файла хранилища (Enter — ${cfg.defaultContainerName}): ")
      target <- show(Boundary.attempt("Путь хранилища") {
        directory.resolve(if filename.isEmpty then cfg.defaultContainerName else filename).toAbsolutePath.normalize
      })
      _ <- show(if Files.exists(target) then Left(AppError.FileExists(target.toString)) else Right(()))
      encrypted <- yes("Зашифровать копии? Для открытия потребуется ключ.")
      // Внешний Option означает успех диалога, внутренний — наличие ключа.
      selectedKey <- if encrypted then newKey(name).map(Some(_)) else Some(None)
      add <- yes("Добавить файлы или папки для копирования сейчас?", true)
      initial <- if add then readSources() else Some(Nil)
    do
      show(Container.create(target, name, encrypted, selectedKey, initial)).foreach { meta =>
        opened = Some(meta)
        key = selectedKey
        event("Создание хранилища")
        println("\nХранилище создано.")
        info(meta)
        println(if initial.isEmpty then "Дальше: пункт 4 — добавьте файлы и папки для копирования." else "Дальше: пункт 6 — сохраните первую резервную копию.")
      }

  private def open(): Unit =
    for
      text <- read("Путь к файлу хранилища: ").filter(_.nonEmpty)
      target <- path(text)
      header <- show(Container.readMeta(target))
      selectedKey <-
        if !header.encrypted then Some(None)
        else
          read("Ключ доступа (Enter — загрузить сохранённый ключ): ").flatMap { entered =>
            if entered.nonEmpty then Some(Some(entered))
            else show(Boundary.attempt("Чтение ключа")(Files.readString(keyFile(header.name)).trim)).map(Some(_))
          }
      meta <- show(Container.open(target, selectedKey))
    do
      opened = Some(meta)
      key = selectedKey
      event("Открытие хранилища")
      info(meta)
      if cfg.checkOnStorageOpen then scan(meta)

  private def scan(meta: StorageMeta): Option[ScanPlan] =
    event("Начало проверки изменений")
    show(Scanner.scan(meta, cfg.hashAlgorithm, cfg.defaultExclusions,
      List(Paths.get(cfg.keyDirectory), Paths.get(cfg.logFile), Config.defaultPath))).map { plan =>
      val r = plan.report
      println(s"Новые: ${r.newFiles.size}; изменённые: ${r.changed.size}; удалённые: ${r.deleted.size}; переименованные: ${r.renamed.size}")
      println(s"Недоступные источники: ${r.unavailableSources.size}; ошибки чтения: ${r.unreadable.size}; новых данных: ${r.totalNewBytes} байт")
      r.newFiles.foreach(f => println(s"+ ${f.rel}"))
      r.changed.foreach((_, f) => println(s"~ ${f.rel}"))
      r.deleted.foreach(f => println(s"- ${f.rel}"))
      r.renamed.foreach((old, f) => println(s"${old.rel} -> ${f.rel}"))
      r.unavailableSources.foreach(println)
      r.unreadable.foreach(println)
      plan.problems.foreach(logError)
      event(s"Проверка завершена: новые=${r.newFiles.size}, изменённые=${r.changed.size}, удалённые=${r.deleted.size}, ошибки=${r.unreadable.size + r.unavailableSources.size}")
      plan
    }

  private def backup(meta: StorageMeta, comment: Option[String]): Unit =
    scan(meta).foreach { plan =>
      if plan.report.isEmpty then println("Изменений нет.")
      else if !cfg.confirmBeforeBackup || yes("Сохранить эти изменения в резервную копию?").contains(true) then
        event("Начало резервного копирования")
        show(Container.backup(meta, key, cfg, plan, comment)).foreach { (next, result) =>
          opened = Some(next)
          println(s"Версия ${result.version.number}: добавлено ${result.added}, обновлено ${result.updated}, удалено ${result.deleted}, пропущено ${result.skipped}, записано ${result.bytes} байт")
          result.problems.foreach(error)
          event(s"Копирование завершено; новая версия ${result.version.number}; успешно=${result.added + result.updated}, пропущено=${result.skipped}")
        }
    }

  private def restore(meta: StorageMeta, selected: Option[Int]): Unit =
    show(selected.orElse(meta.lastVersion.map(_.number)).toRight(AppError.NoVersions)).foreach { number =>
      meta.versions.find(_.number == number).foreach(v => println(s"Результат сохранения версии: ${v.result}"))
      for
        text <- read("Каталог восстановления: ").filter(_.nonEmpty)
        destination <- path(text)
        _ <- show(Boundary.attempt("Создание каталога восстановления")(Files.createDirectories(destination)))
        conflicts <- show(Container.restoreConflicts(meta, number, destination))
      do
        val overwrite = conflicts.filter { rel =>
          running && (!cfg.confirmBeforeRestoreOverwrite || yes(s"Файл $rel уже существует. Заменить его?").contains(true))
        }.toSet
        if running then
          show(Container.restore(meta, key, number, destination, overwrite)).foreach { result =>
            println(s"Восстановлено: ${result.restored}; пропущено: ${result.skipped}")
            result.problems.foreach(error)
            event(s"Восстановление версии $number: успешно=${result.restored}, пропущено=${result.skipped}")
          }
    }

  private def execute(command: Command): Unit = command match
    case Command.Help => println(Menu.help)
    case Command.Create => create()
    case Command.Open => open()
    case Command.Info => withStorage(info)
    case Command.Sources(action) => withStorage(meta => changeSources(meta, action))
    case Command.Scan => withStorage(meta => { scan(meta); () })
    case Command.Backup(comment) => withStorage { meta =>
      val note = comment.orElse(read("Комментарий к копии (Enter — без комментария): ").filter(_.nonEmpty))
      if running then backup(meta, note)
    }
    case Command.Versions => withStorage { meta =>
      if meta.versions.isEmpty then println("Копий пока нет. Создайте первую через пункт 6.")
      else meta.versions.foreach(v => println(v.summary))
    }
    case Command.Restore(version) => withStorage { meta =>
      if meta.versions.isEmpty then error(AppError.NoVersions)
      else version match
        case Some(_) => restore(meta, version)
        case None =>
          meta.versions.foreach(v => println(v.summary))
          read("Номер версии (Enter — последняя): ").foreach { text =>
            if text.isEmpty then restore(meta, None)
            else show(Command.positiveInt(text).toRight(AppError.InvalidInput("Нужен номер версии")))
              .foreach(n => restore(meta, Some(n)))
          }
    }
    case Command.Verify => withStorage { meta =>
      show(Container.verify(meta, key)).foreach { r =>
        println(s"Целых блоков: ${r.ok}; повреждено: ${r.damaged.size}; отсутствует: ${r.missing.size}")
        r.damaged.foreach(h => println(s"Повреждён: $h"))
        r.missing.foreach(h => println(s"Отсутствует: $h"))
        event(s"Проверка целостности: целых=${r.ok}, повреждений=${r.damaged.size + r.missing.size}")
      }
    }
    case Command.Reload => show(Config.load(Config.defaultPath)).foreach { next =>
      cfg = next
      log = new Logger(cfg.logFile)
      event("Конфигурация загружена и проверена повторно")
      println("Конфигурация перезагружена.")
    }
    case Command.Exit => running = false

  def run(): Unit =
    event("Запуск приложения; конфигурация загружена и проверена")
    while running do
      println("\n=== РЕЗЕРВНОЕ КОПИРОВАНИЕ ===")
      println(opened.map(meta => s"Открыто хранилище: ${meta.name}").getOrElse("Хранилище не открыто. Начните с пункта 1 или 2."))
      println(Menu.help)
      read("Выберите пункт (0–11): ").filter(_.nonEmpty).foreach(line => show(Command.parse(line)).foreach(execute))
    event("Завершение приложения")

object Menu:

  val help = """1. Создать новое хранилище
2. Открыть существующее хранилище
3. Сведения о хранилище
4. Выбрать файлы и папки для копирования
5. Показать изменения файлов
6. Сохранить резервную копию
7. Показать сохранённые версии
8. Восстановить файлы из копии
9. Проверить целостность копий
10. Перезагрузить настройки из backup.json
11. Показать меню
0. Выйти из программы
Можно также вводить команды: create, open, info, sources, scan,
backup [комментарий], versions, restore [номер], verify, reload, exit."""
