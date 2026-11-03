package backup

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.io.StdIn
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

final class Menu(initialConfig: Config):
  private var cfg = initialConfig
  private var log = new Logger(cfg.logFile)
  private var opened: Option[StorageMeta] = None
  private var key: Option[String] = None
  private var running = true

  private def event(message: String): Unit =
    log.info(message) match
      case Left(problem) => Console.err.println(problem.message)
      case Right(_) => ()

  private def logError(problem: AppError): Unit =
    // Сообщения об ошибках не содержат ключей и содержимого файлов.
    event(s"Ошибка: ${problem.message}")

  private def error(problem: AppError): Unit =
    println(problem.message)
    logError(problem)

  private def read(prompt: String): String =
    print(prompt)
    val text = StdIn.readLine()
    // null приходит только из readLine при закрытии входного потока.
    // Как в примере с пары, запоминаем это в running и отменяем диалог.
    if text == null then
      running = false
      ""
    else text.trim

  private def yes(question: String, default: Boolean = false): Boolean =
    println(s"$question  1 — Да, 2 — Нет (Enter — ${if default then "Да" else "Нет"})")
    var answer = false
    var done = false
    while running && !done do
      read("Ваш ответ: ").toLowerCase match
        case "" =>
          answer = default
          done = true
        case "да" | "1" =>
          answer = true
          done = true
        case "нет" | "2" =>
          answer = false
          done = true
        case _ => println("Введите 1 (да), 2 (нет) или нажмите Enter.")
    running && answer

  private def readNumber(prompt: String): Option[Int] =
    var number: Option[Int] = None
    var done = false
    while running && !done do
      val text = read(prompt)
      if text.isEmpty then done = true
      else
        number = Command.positiveInt(text)
        if number.nonEmpty then done = true
        else println("Введите целый положительный номер. Enter — отмена.")
    number

  private def path(text: String): Option[Path] =
    try Some(Paths.get(text).toAbsolutePath.normalize())
    catch
      case NonFatal(_) =>
        println("Неправильный путь.")
        None

  private def info(meta: StorageMeta): Unit =
    println(s"Хранилище: ${meta.name}\nФайл: ${meta.containerPath}")
    println(s"Шифрование: ${if meta.encrypted then "включено" else "выключено"}")
    println(s"Источников: ${meta.sources.count(source => !source.removed)}")
    println(s"Последняя копия: ${meta.lastVersion.map(_.timestamp).getOrElse("нет")}")
    println(s"Сохранённых версий: ${meta.versions.size}")

  private def sources(meta: StorageMeta): Unit =
    if meta.sources.forall(_.removed) then println("Источников пока нет.")
    for index <- meta.sources.indices do
      val source = meta.sources(index)
      if !source.removed then
        println(s"${index + 1}. ${source.path}")
        println(s"   Вложенные папки: ${if source.includeSubdirs then "да" else "нет"}; копировать: ${if source.enabled then "да" else "нет"}; ${source.status}")
        println(s"   Исключения: ${source.exclusions.mkString(", ")}; последняя копия: ${source.lastBackup.getOrElse("нет")}")

  private def readSource(): Option[Source] =
    val text = read("Путь к файлу или каталогу (Enter — отмена): ")
    if !running || text.isEmpty then None
    else path(text) match
      case None => None
      case Some(sourcePath) =>
        val recursive = yes("Копировать также вложенные папки?", true)
        if !running then None
        else
          val exclusions = read("Исключения через запятую (Enter — из конфигурации): ")
          val words = exclusions.split(",").toList
          val patterns = if exclusions.isEmpty then cfg.defaultExclusions else words.map(word => word.trim)
          if !running then None
          else Source.validate(Source(sourcePath.toString, recursive, patterns)) match
            case Left(problem) =>
              error(problem)
              None
            case Right(source) => Some(source)

  private def readSources(): Option[List[Source]] =
    var result = List.empty[Source]
    var more = true
    var cancelled = false
    while running && more do
      readSource() match
        case Some(source) =>
          result = result :+ source
          more = yes("Добавить ещё файл или папку?")
        case None =>
          cancelled = true
          more = false
    if running && !cancelled then Some(result) else None

  private def saveSources(meta: StorageMeta, updated: List[Source]): Unit =
    val next = meta.copy(sources = updated)
    Container.save(next, key) match
      case Left(problem) => error(problem)
      case Right(_) =>
        opened = Some(next)
        event("Изменение списка источников")
        sources(next)

  private def changeSources(meta: StorageMeta, action: SourceAction): Unit = action match
    case SourceAction.Menu => sourcesMenu()
    case SourceAction.ListAll => sources(meta)
    case SourceAction.Add =>
      readSource() match
        case Some(source) => saveSources(meta, meta.sources :+ source)
        case None => ()
    case SourceAction.Edit(number) =>
      meta.sources.lift(number - 1) match
        case Some(old) if !old.removed =>
          readSource() match
            case Some(source) =>
              val lastBackup = if source.path == old.path then old.lastBackup else None
              val updated = source.copy(enabled = old.enabled, lastBackup = lastBackup)
              saveSources(meta, meta.sources.updated(number - 1, updated))
            case None => ()
        case _ => error(AppError.SourceNotFound(number))
    case SourceAction.Remove(number) =>
      meta.sources.lift(number - 1) match
        case Some(source) if !source.removed =>
          val updated = source.copy(enabled = false, removed = true)
          saveSources(meta, meta.sources.updated(number - 1, updated))
        case _ => error(AppError.SourceNotFound(number))
    case SourceAction.SetEnabled(number, enabled) =>
      meta.sources.lift(number - 1) match
        case Some(source) if !source.removed =>
          val updated = source.copy(enabled = enabled)
          saveSources(meta, meta.sources.updated(number - 1, updated))
        case _ => error(AppError.SourceNotFound(number))

  private def sourcesMenu(): Unit =
    var stay = true
    while stay && running do
      opened match
        case None =>
          error(AppError.NoStorage)
          stay = false
        case Some(meta) =>
          println("\n=== ФАЙЛЫ И ПАПКИ ДЛЯ КОПИРОВАНИЯ ===")
          sources(meta)
          println("\n1. Добавить файл или папку\n2. Изменить источник\n3. Удалить из списка\n4. Включить копирование\n5. Отключить копирование\n0. Назад в главное меню")
          val choice = read("Выберите действие (0–5): ")
          if running then choice match
            case "0" | "" => stay = false
            case "1" => changeSources(meta, SourceAction.Add)
            case "2" | "3" | "4" | "5" =>
              readNumber("Номер источника (Enter — отмена): ") match
                case None => ()
                case Some(number) =>
                  val action = choice match
                    case "2" => SourceAction.Edit(number)
                    case "3" => SourceAction.Remove(number)
                    case "4" => SourceAction.SetEnabled(number, true)
                    case _ => SourceAction.SetEnabled(number, false)
                  changeSources(meta, action)
            case _ => println("Такого пункта нет. Выберите номер от 0 до 5.")

  private def chooseDirectory(): Option[Path] =
    val roots = java.nio.file.FileSystems.getDefault.getRootDirectories.asScala.toList
    event("Обнаружение доступных устройств")
    println("\nГде сохранить файл хранилища? Диски:")
    for index <- roots.indices do
      println(s"${index + 1}. ${roots(index)}")
    var selected: Option[Path] = None
    var done = false
    while running && !done do
      val text = read("Номер диска или полный путь к папке (Enter — отмена): ")
      if text.isEmpty then done = true
      else
        val candidate = Command.positiveInt(text) match
          case Some(number) => roots.lift(number - 1)
          case None => path(text)
        candidate match
          case Some(directory) if Files.isDirectory(directory) && Files.isWritable(directory) =>
            selected = Some(directory)
            done = true
          case _ => println("Укажите существующую папку, в которую можно записать файл.")
    selected

  private def keyFile(name: String): Path =
    Paths.get(cfg.keyDirectory).resolve(name.replaceAll("[\\\\/:*?\"<>|]", "_") + ".key")

  private def newKey(name: String): Option[String] =
    val generated = Crypto.generateKey()
    println(s"Как хранить ключ?\n1. Сохранить в каталоге ${cfg.keyDirectory}\n2. Показать ключ, чтобы вы сохранили его сами")
    val choice = read("Выберите 1 или 2 (Enter — отмена): ")
    if !running then None
    else choice match
      case "1" =>
        try
          val file = keyFile(name)
          val folder = file.toAbsolutePath.getParent
          // Java создаёт каталог ключей вместе с недостающими родителями.
          Files.createDirectories(folder)
          Files.writeString(file, generated, StandardOpenOption.CREATE_NEW)
          println(s"Ключ сохранён: $file. Потеря ключа сделает хранилище недоступным.")
          Some(generated)
        catch
          case NonFatal(_) =>
            error(AppError.WriteFailed(cfg.keyDirectory))
            None
      case "2" =>
        println(s"Ключ: $generated\nПрограмма его не сохранит. Без ключа открыть хранилище невозможно.")
        if yes("Вы сохранили показанный ключ?") then Some(generated) else None
      case "" => None
      case _ =>
        println("Нужно выбрать 1 или 2.")
        None

  private def create(): Unit =
    println("\n=== СОЗДАНИЕ ХРАНИЛИЩА ===")
    chooseDirectory() match
      case Some(directory) => createIn(directory)
      case None => ()

  private def createIn(directory: Path): Unit =
    val name = read("Название хранилища: ")
    if !running || name.isEmpty then return
    val enteredName = read(s"Имя файла (Enter — ${cfg.defaultContainerName}): ")
    if !running then return
    val filename = if enteredName.isEmpty then cfg.defaultContainerName else enteredName
    path(directory.toString + "/" + filename).foreach { target =>
      if Files.exists(target) then error(AppError.FileExists(target.toString))
      else
        val encrypted = yes("Зашифровать копии? Для открытия потребуется ключ.")
        val selectedKey = if running && encrypted then newKey(name) else None
        if running && (!encrypted || selectedKey.nonEmpty) then
          val add = yes("Добавить файлы или папки для копирования сейчас?", true)
          val initial = if add then readSources() else Some(List.empty[Source])
          if running then initial.foreach { list =>
            Container.create(target, name, encrypted, selectedKey, list) match
              case Left(problem) => error(problem)
              case Right(meta) =>
                opened = Some(meta)
                key = selectedKey
                event("Создание хранилища")
                println("Хранилище создано.")
                info(meta)
          }
    }

  private def savedKey(name: String): Option[String] =
    try Some(Files.readString(keyFile(name)).trim)
    catch
      case NonFatal(_) =>
        error(AppError.ReadFailed(cfg.keyDirectory))
        None

  private def open(): Unit =
    val text = read("Путь к файлу хранилища (Enter — отмена): ")
    if running && text.nonEmpty then path(text).foreach { target =>
      Container.readMeta(target) match
        case Left(problem) => error(problem)
        case Right(header) =>
          var selectedKey: Option[String] = None
          if header.encrypted then
            val entered = read("Ключ доступа (Enter — загрузить сохранённый ключ): ")
            if running then selectedKey = if entered.isEmpty then savedKey(header.name) else Some(entered)
          if running && (!header.encrypted || selectedKey.nonEmpty) then
            Container.open(target, selectedKey) match
              case Left(problem) => error(problem)
              case Right(meta) =>
                opened = Some(meta)
                key = selectedKey
                event("Открытие хранилища")
                info(meta)
                if cfg.checkOnStorageOpen then scan(meta)
    }

  private def scan(meta: StorageMeta): Option[ScanPlan] =
    event("Начало проверки изменений")
    Scanner.scan(meta, cfg.hashAlgorithm, cfg.defaultExclusions,
      List(Paths.get(cfg.keyDirectory), Paths.get(cfg.logFile), Config.defaultPath)) match
      case Left(problem) =>
        error(problem)
        None
      case Right(plan) =>
        val report = plan.report
        println(s"Новые: ${report.newFiles.size}; изменённые: ${report.changed.size}; удалённые: ${report.deleted.size}; переименованные: ${report.renamed.size}")
        println(s"Недоступные источники: ${report.unavailableSources.size}; ошибки чтения: ${report.unreadable.size}; новых данных: ${report.totalNewBytes} байт")
        for file <- report.newFiles do println(s"+ ${file.rel}")
        for (_, file) <- report.changed do println(s"~ ${file.rel}")
        for file <- report.deleted do println(s"- ${file.rel}")
        for (old, file) <- report.renamed do println(s"${old.rel} -> ${file.rel}")
        for source <- report.unavailableSources do println(source)
        for file <- report.unreadable do println(file)
        for problem <- plan.problems do logError(problem)
        event(s"Проверка завершена: новые=${report.newFiles.size}, изменённые=${report.changed.size}, удалённые=${report.deleted.size}, ошибки=${plan.problems.size}")
        Some(plan)

  private def backup(meta: StorageMeta, comment: Option[String]): Unit =
    scan(meta) match
      case None => ()
      case Some(plan) =>
        if plan.report.isEmpty then println("Изменений нет.")
        else
          val confirmed = !cfg.confirmBeforeBackup || yes("Сохранить эти изменения в резервную копию?")
          if running && confirmed then
            event("Начало резервного копирования")
            Container.backup(meta, key, cfg, plan, comment) match
              case Left(problem) => error(problem)
              case Right((next, result)) =>
                opened = Some(next)
                println(s"Версия ${result.version.number}: добавлено ${result.added}, обновлено ${result.updated}, удалено ${result.deleted}, пропущено ${result.skipped}, записано ${result.bytes} байт")
                for problem <- result.problems do error(problem)
                event(s"Копирование завершено; новая версия ${result.version.number}; успешно=${result.added + result.updated}, пропущено=${result.skipped}")

  private def restore(meta: StorageMeta, selected: Option[Int]): Unit =
    val version = selected match
      case Some(_) => selected
      case None => meta.lastVersion.map(value => value.number)
    version match
      case None => error(AppError.NoVersions)
      case Some(number) =>
        for value <- meta.versions.find(value => value.number == number) do
          println(s"Результат сохранения версии: ${value.result}")
        val text = read("Каталог восстановления (Enter — отмена): ")
        if running && text.nonEmpty then path(text).foreach { destination =>
          prepareDirectory(destination).flatMap { _ =>
            Container.restoreConflicts(meta, number, destination)
          } match
            case Left(problem) => error(problem)
            case Right(conflicts) =>
              var overwrite = List.empty[String]
              for rel <- conflicts do
                if running && (!cfg.confirmBeforeRestoreOverwrite || yes(s"Файл $rel уже существует. Заменить его?")) then
                  overwrite = overwrite :+ rel
              if running then
                Container.restore(meta, key, number, destination, overwrite) match
                  case Left(problem) => error(problem)
                  case Right(result) =>
                    println(s"Восстановлено: ${result.restored}; пропущено: ${result.skipped}")
                    for problem <- result.problems do error(problem)
                    event(s"Восстановление версии $number: успешно=${result.restored}, пропущено=${result.skipped}")
        }

  private def prepareDirectory(directory: Path): Either[AppError, Unit] =
    try
      // Java создаёт каталог восстановления вместе с недостающими родителями.
      Files.createDirectories(directory)
      Right(())
    catch
      case NonFatal(_) => Left(AppError.DirectoryCreateFailed(directory.toString))

  private def execute(command: Command): Unit = command match
    case Command.Help => println(Menu.help)
    case Command.Create => create()
    case Command.Open => open()
    case Command.Reload => Config.load(Config.defaultPath) match
      case Left(problem) => error(problem)
      case Right(next) =>
        cfg = next
        log = new Logger(cfg.logFile)
        event("Конфигурация загружена и проверена повторно")
        println("Конфигурация перезагружена.")
    case Command.Exit => running = false
    case _ =>
      opened match
        case None => error(AppError.NoStorage)
        case Some(meta) => command match
          case Command.Info => info(meta)
          case Command.Sources(action) => changeSources(meta, action)
          case Command.Scan => scan(meta)
          case Command.Backup(comment) =>
            var note = comment
            if note.isEmpty then
              val text = read("Комментарий к копии (Enter — без комментария): ")
              if text.nonEmpty then note = Some(text)
            if running then backup(meta, note)
          case Command.Versions =>
            if meta.versions.isEmpty then println("Копий пока нет. Создайте первую через пункт 6.")
            else
              for version <- meta.versions do println(version.summary)
          case Command.Restore(version) =>
            if meta.versions.isEmpty then error(AppError.NoVersions)
            else version match
              case Some(_) => restore(meta, version)
              case None =>
                for value <- meta.versions do println(value.summary)
                val text = read("Номер версии (Enter — последняя): ")
                if running then
                  if text.isEmpty then restore(meta, None)
                  else Command.positiveInt(text) match
                    case Some(number) => restore(meta, Some(number))
                    case None => println("Нужен целый положительный номер версии.")
          case Command.Verify =>
            Container.verify(meta, key) match
              case Left(problem) => error(problem)
              case Right(result) =>
                println(s"Целых блоков: ${result.ok}; повреждено: ${result.damaged.size}; отсутствует: ${result.missing.size}")
                for hash <- result.damaged do println(s"Повреждён: $hash")
                for hash <- result.missing do println(s"Отсутствует: $hash")
                event(s"Проверка целостности: целых=${result.ok}, повреждений=${result.damaged.size + result.missing.size}")
          case _ => ()

  def run(): Unit =
    event("Запуск приложения; конфигурация загружена и проверена")
    while running do
      println("\n=== РЕЗЕРВНОЕ КОПИРОВАНИЕ ===")
      println(opened.map(meta => s"Открыто хранилище: ${meta.name}").getOrElse("Хранилище не открыто. Начните с пункта 1 или 2."))
      println(Menu.help)
      val line = read("Выберите пункт (0–11): ")
      if running && line.nonEmpty then Command.parse(line) match
        case Left(problem) => error(problem)
        case Right(command) => execute(command)
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
