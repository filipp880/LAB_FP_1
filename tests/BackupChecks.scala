package backup

import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import scala.jdk.CollectionConverters.*

// Интеграционные проверки используют отдельную временную папку.
@main def backupChecks(): Unit =
  val root = Files.createTempDirectory("backup-checks-")
  var passed = 0
  def check(name: String)(body: => Unit): Unit =
    body
    passed += 1
    println(s"OK: $name")
  def value[A](result: Either[AppError, A]): A = result match
    case Right(a) => a
    case Left(e) => throw new AssertionError(s"Ожидался Right, получен $e; данные: $root")
  def put(path: Path, text: String): Unit =
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)
  def text(path: Path): String = Files.readString(path)
  def blobNames(path: Path): List[String] =
    val zip = new ZipFile(path.toFile)
    try zip.entries().asScala.map(_.getName).filter(_.startsWith("blobs/")).toList
    finally zip.close()
  // Намеренно повреждает один блок, сохраняя корректную структуру ZIP.
  def rewriteBlob(path: Path): Unit =
    val zip = new ZipFile(path.toFile)
    val tmp = path.resolveSibling(path.getFileName.toString + ".test-tmp")
    try
      val out = new ZipOutputStream(Files.newOutputStream(tmp))
      try
        var changed = false
        zip.entries().asScala.foreach { entry =>
          val in = zip.getInputStream(entry)
          val bytes = try in.readAllBytes() finally in.close()
          if !changed && entry.getName.startsWith("blobs/") then
            bytes(bytes.length - 1) = (bytes.last ^ 1).toByte
            changed = true
          out.putNextEntry(new ZipEntry(entry.getName))
          out.write(bytes)
          out.closeEntry()
        }
      finally out.close()
    finally zip.close()
    Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)

  val cfg = Config.Default.copy(
    logFile = root.resolve("backup.log").toString,
    keyDirectory = root.resolve("keys").toString,
    defaultExclusions = List("*.tmp", "cache"),
    maximumVersionsPerFile = 3
  )
  val source = Files.createDirectory(root.resolve("source"))
  val store = root.resolve("plain.store")
  def scan(meta: StorageMeta, config: Config = cfg): ScanPlan =
    value(Scanner.scan(meta, config.hashAlgorithm, config.defaultExclusions))
  def backup(meta: StorageMeta, config: Config = cfg): (StorageMeta, BackupResult) =
    value(Container.backup(meta, None, config, scan(meta, config), None))

  check("конфигурация: валидация и JSON round-trip") {
    value(Config.validate(cfg))
    val path = root.resolve("backup.json")
    value(Config.save(path, cfg))
    assert(value(Config.load(path)) == cfg)
    assert(Config.validate(cfg.copy(maximumVersionsPerFile = 0)).isLeft)
    assert(Config.validate(cfg.copy(logFile = "")).isLeft)
    assert(Config.validate(cfg.copy(hashAlgorithm = "unknown")).isLeft)
    assert(Config.validate(cfg.copy(defaultExclusions = List("[bad"))).isLeft)
    put(path, "{broken")
    assert(Config.load(path).isLeft)
  }
  check("конфигурация: null и неправильные типы возвращают Left") {
    val path = root.resolve("malformed-config.json")
    val invalid: List[(String, ujson.Value)] = List(
      "logFile" -> ujson.Null,
      "keyDirectory" -> ujson.Null,
      "defaultContainerName" -> ujson.Null,
      "hashAlgorithm" -> ujson.Null,
      "defaultExclusions" -> ujson.Null,
      "defaultExclusions" -> ujson.Arr(1),
      "defaultExclusions" -> ujson.Arr(ujson.Null),
      "defaultExclusions" -> ujson.Str("*.tmp"),
      "maximumVersionsPerFile" -> ujson.Str("10"),
      "checkOnStorageOpen" -> ujson.Str("yes")
    )
    invalid.foreach { case (field, replacement) =>
      val json = Config.toJson(cfg)
      json(field) = replacement
      put(path, ujson.write(json))
      assert(Config.load(path).isLeft, s"$field=$replacement")
    }
  }
  check("журнал дополняется между запусками") {
    val path = root.resolve("append.log")
    value(new Logger(path.toString).info("first run"))
    value(new Logger(path.toString).info("second run"))
    val lines = Files.readAllLines(path).asScala.toList
    assert(lines.size == 2)
    assert(lines.head.contains("first run") && lines.last.contains("second run"))
    assert(new Logger(root.toString).info("unwritable target").isLeft)
  }
  check("команды разбираются в ADT") {
    List("create", "open", "info", "sources", "scan", "backup", "versions", "restore", "verify", "reload", "exit")
      .foreach(line => assert(Command.parse(line).isRight, line))
    assert(Command.parse("no-such-command").isLeft)
  }

  var meta = value(Container.create(store, "Проверка", false, None,
    List(Source(source.toString, true, Nil))))
  check("создание: существующий файл не затирается") {
    assert(Container.create(store, "Другое", false, None, Nil).isLeft)
    assert(value(Container.open(store, None)).name == "Проверка")
    assert(Container.open(root.resolve("absent.store"), None).isLeft)
    put(root.resolve("broken.store"), "not a zip")
    assert(Container.readMeta(root.resolve("broken.store")).isLeft)
    assert(StorageMeta.fromJson(ujson.Obj("format" -> 2), store).isLeft)
  }
  put(source.resolve("a.txt"), "A")
  put(source.resolve("b.txt"), "A")
  put(source.resolve("sub/deep.txt"), "D")
  put(source.resolve("ignore.tmp"), "ignored")
  put(source.resolve("cache/ignored.txt"), "ignored")
  check("сканирование: рекурсия и исключения") {
    val plan = scan(meta)
    assert(plan.report.newFiles.map(_.rel).toSet == Set("s0/a.txt", "s0/b.txt", "s0/sub/deep.txt"))
    assert(plan.report.totalNewBytes == 3)
    val shallow = meta.copy(sources = meta.sources.map(_.copy(includeSubdirs = false)))
    assert(scan(shallow).report.newFiles.map(_.rel).toSet == Set("s0/a.txt", "s0/b.txt"))
  }
  val first = backup(meta)
  meta = first._1
  check("backup: счётчики, дедупликация, метаданные и целостность") {
    assert(first._2.added == 3 && first._2.skipped == 0 && first._2.bytes == 3)
    assert(blobNames(store).size == 2)
    value(Container.save(meta, None))
    val reopened = value(Container.open(store, None))
    assert(reopened.versions == meta.versions)
    assert(reopened.sources.head.lastBackup.nonEmpty)
    val verified = value(Container.verify(meta, None))
    assert(verified.ok == 2 && verified.damaged.isEmpty && verified.missing.isEmpty)
    assert(scan(meta).report.isEmpty)
  }
  check("совместимость: контейнер ЛР0 без шифрования и явный отказ от старого XOR") {
    val legacy = StorageMeta.toJson(meta)
    legacy("format") = 1
    legacy("keyCheck") = ""
    legacy.obj.remove("cipher")
    legacy("sources").arr.foreach(_.obj.remove("removed"))
    val legacyPath = root.resolve("legacy.store")
    val zip = new ZipFile(store.toFile)
    try
      val out = new ZipOutputStream(Files.newOutputStream(legacyPath))
      try zip.entries().asScala.foreach { entry =>
        out.putNextEntry(new ZipEntry(entry.getName))
        if entry.getName == "meta/filerecords.json" then out.write(ujson.write(legacy).getBytes(java.nio.charset.StandardCharsets.UTF_8))
        else
          val in = zip.getInputStream(entry)
          try in.transferTo(out)
          finally in.close()
        out.closeEntry()
      }
      finally out.close()
    finally zip.close()
    val opened = value(Container.open(legacyPath, None))
    assert(opened.versions == meta.versions)
    val destination = Files.createDirectory(root.resolve("restore-legacy"))
    value(Container.restore(opened, None, 1, destination))
    assert(text(destination.resolve("s0/a.txt")) == "A")
    legacy("encrypted") = true
    legacy("keyCheck") = "legacy-check"
    assert(StorageMeta.fromJson(legacy, legacyPath).left.toOption.exists {
      case AppError.DamagedStorage(reason) => reason.contains("ЛР0")
      case _ => false
    })
  }
  check("ключи и служебные файлы не включаются в контейнер") {
    val directory = Files.createDirectory(root.resolve("protected-source"))
    val keys = directory.resolve("keys")
    val secretKey = Crypto.generateKey()
    val logPath = directory.resolve("backup.log")
    put(keys.resolve("saved.key"), secretKey)
    put(logPath, "log content")
    put(directory.resolve("ordinary.txt"), "ordinary")
    val protectedCfg = cfg.copy(keyDirectory = keys.toString, logFile = logPath.toString, defaultExclusions = Nil)
    val protectedMeta = value(Container.create(root.resolve("protected.store"), "Protected", false, None,
      List(Source(directory.toString, true, Nil))))
    val protectedPaths = List(keys, logPath, Config.defaultPath)
    val safe = value(Scanner.scan(protectedMeta, cfg.hashAlgorithm, Nil, protectedPaths))
    assert(safe.report.newFiles.map(_.rel).toSet == Set("s0/ordinary.txt"))

    val unsafePlan = value(Scanner.scan(protectedMeta, cfg.hashAlgorithm, Nil))
    assert(unsafePlan.report.newFiles.exists(_.rel == "s0/keys/saved.key"))
    val (saved, result) = value(Container.backup(protectedMeta, None, protectedCfg, unsafePlan, None))
    assert(result.skipped >= 2 && result.added == 1)
    assert(saved.lastVersion.exists(_.snapshot.keySet == Set("s0/ordinary.txt")))
    val archive = new ZipFile(saved.containerPath.toFile)
    try archive.entries().asScala.foreach { entry =>
      val in = archive.getInputStream(entry)
      val bytes = try in.readAllBytes() finally in.close()
      assert(!new String(bytes, java.nio.charset.StandardCharsets.UTF_8).contains(secretKey))
    }
    finally archive.close()
  }
  put(source.resolve("copy.txt"), "A")
  meta = backup(meta)._1
  check("повторный blob не создаёт дубликат ZIP entry") {
    val names = blobNames(store)
    assert(names.size == 2 && names.distinct == names)
  }

  put(source.resolve("a.txt"), "A2")
  Files.delete(source.resolve("b.txt"))
  Files.move(source.resolve("sub/deep.txt"), source.resolve("moved.txt"))
  check("сканирование: изменение, удаление и переименование") {
    val report = scan(meta).report
    assert(report.changed.map(_._2.rel) == List("s0/a.txt"))
    assert(report.deleted.exists(_.rel == "s0/b.txt"))
    assert(report.renamed.exists { case (old, current) =>
      old.rel == "s0/sub/deep.txt" && current.rel == "s0/moved.txt"
    })
  }
  meta = backup(meta)._1
  check("восстановление выбранной и последней версии") {
    val oldDest = Files.createDirectory(root.resolve("restore-old"))
    val latestDest = Files.createDirectory(root.resolve("restore-latest"))
    assert(value(Container.restore(meta, None, 1, oldDest)).problems.isEmpty)
    assert(text(oldDest.resolve("s0/a.txt")) == "A")
    assert(text(oldDest.resolve("s0/b.txt")) == "A")
    assert(text(oldDest.resolve("s0/sub/deep.txt")) == "D")
    val last = meta.lastVersion.toList.map(_.number).head
    assert(value(Container.restore(meta, None, last, latestDest)).problems.isEmpty)
    assert(text(latestDest.resolve("s0/a.txt")) == "A2")
    assert(text(latestDest.resolve("s0/moved.txt")) == "D")
    assert(!Files.exists(latestDest.resolve("s0/b.txt")))
    put(latestDest.resolve("s0/a.txt"), "local edits")
    assert(value(Container.restoreConflicts(meta, last, latestDest)).contains("s0/a.txt"))
    value(Container.restore(meta, None, last, latestDest))
    assert(text(latestDest.resolve("s0/a.txt")) == "local edits")
    value(Container.restore(meta, None, last, latestDest, Set("s0/a.txt")))
    assert(text(latestDest.resolve("s0/a.txt")) == "A2")
    assert(Container.restore(meta, None, 999, latestDest).isLeft)
  }
  check("недоступный, отключённый и удалённый источник не означает удаление файлов") {
    val disabled = meta.copy(sources = meta.sources.map(_.copy(enabled = false)))
    val removed = meta.copy(sources = meta.sources.map(_.copy(enabled = false, removed = true)))
    assert(scan(disabled).report.deleted.isEmpty)
    assert(scan(removed).report.deleted.isEmpty)
    val movedSource = root.resolve("source-offline")
    Files.move(source, movedSource)
    try
      val report = scan(meta).report
      assert(report.unavailableSources.nonEmpty && report.deleted.isEmpty)
    finally Files.move(movedSource, source)
  }
  check("изменение файла между scan и backup не считается успехом") {
    put(source.resolve("a.txt"), "planned")
    val plan = scan(meta)
    put(source.resolve("a.txt"), "changed after scan")
    val (updated, result) = value(Container.backup(meta, None, cfg, plan, None))
    assert(result.skipped >= 1 && result.updated == 0)
    assert(result.problems.exists { case AppError.FileChanged(_) => true; case _ => false })
    val before = meta.lastVersion.toList.flatMap(_.snapshot.get("s0/a.txt"))
    val after = updated.lastVersion.toList.flatMap(_.snapshot.get("s0/a.txt"))
    assert(before == after)
    meta = updated
  }
  check("лимит относится к версиям каждого файла и сохраняет последнее содержимое") {
    val limited = cfg.copy(maximumVersionsPerFile = 2)
    List("v1", "v2", "v3", "v4").foreach { content =>
      put(source.resolve("a.txt"), content)
      meta = backup(meta, limited)._1
    }
    val refs = meta.versions.flatMap(_.snapshot.get("s0/a.txt").flatMap(_.hash)).distinct
    assert(refs.size <= 2)
    val unchanged = meta.versions.flatMap(_.snapshot.get("s0/copy.txt").flatMap(_.hash))
    assert(unchanged.nonEmpty)
    val destination = Files.createDirectory(root.resolve("restore-limited"))
    val last = meta.lastVersion.toList.map(_.number).head
    value(Container.restore(meta, None, last, destination))
    assert(text(destination.resolve("s0/a.txt")) == "v4")
    assert(text(destination.resolve("s0/copy.txt")) == "A")
    assert(value(Container.verify(meta, None)).damaged.isEmpty)
  }
  check("отдельный файл как источник правильно находится и восстанавливается") {
    val file = root.resolve("single.txt")
    put(file, "single")
    val single = value(Container.create(root.resolve("single.store"), "Single", false, None,
      List(Source(file.toString, false, Nil))))
    val saved = backup(single)._1
    val destination = Files.createDirectory(root.resolve("restore-single"))
    value(Container.restore(saved, None, 1, destination))
    assert(text(destination.resolve("s0/single.txt")) == "single")
  }
  check("смена алгоритма не мешает проверке и восстановлению старых данных") {
    val directory = Files.createDirectory(root.resolve("mixed-source"))
    put(directory.resolve("old.txt"), "old hash")
    var mixed = value(Container.create(root.resolve("mixed.store"), "Mixed", false, None,
      List(Source(directory.toString, true, Nil))))
    mixed = backup(mixed)._1
    val md5 = cfg.copy(hashAlgorithm = "MD5")
    put(directory.resolve("new.txt"), "new hash")
    mixed = backup(mixed, md5)._1
    val report = value(Container.verify(mixed, None))
    assert(report.damaged.isEmpty && report.missing.isEmpty)
    val destination = Files.createDirectory(root.resolve("restore-mixed"))
    val last = mixed.lastVersion.toList.map(_.number).head
    value(Container.restore(mixed, None, last, destination))
    assert(text(destination.resolve("s0/old.txt")) == "old hash")
    assert(text(destination.resolve("s0/new.txt")) == "new hash")
  }
  check("шифрование: верный/неверный ключ, повреждение и безопасное восстановление") {
    val file = root.resolve("secret.txt")
    put(file, "secret content")
    val path = root.resolve("encrypted.store")
    val key = Some(Crypto.generateKey())
    val empty = value(Container.create(path, "Encrypted", true, key, List(Source(file.toString, false, Nil))))
    val encrypted = value(Container.backup(empty, key, cfg, scan(empty), Some("test")))._1
    assert(Container.open(path, None) == Left(AppError.KeyRequired))
    assert(Container.open(path, Some("wrong")) == Left(AppError.WrongKey))
    assert(value(Container.open(path, key)).encrypted)
    assert(value(Container.verify(encrypted, key)).ok == 1)
    val destination = Files.createDirectory(root.resolve("restore-encrypted"))
    value(Container.restore(encrypted, key, 1, destination))
    assert(text(destination.resolve("s0/secret.txt")) == "secret content")
    rewriteBlob(path)
    val damaged = Container.verify(encrypted, key)
    assert(damaged.isLeft || damaged.toOption.exists(_.damaged.nonEmpty))
    put(destination.resolve("s0/secret.txt"), "keep this")
    val restored = value(Container.restore(encrypted, key, 1, destination, Set("s0/secret.txt")))
    assert(restored.problems.nonEmpty)
    assert(text(destination.resolve("s0/secret.txt")) == "keep this")
  }
  check("перезагрузка: отсутствующая конфигурация не заменяется настройками по умолчанию") {
    val absent = root.resolve("missing-config.json")
    assert(Config.load(absent) == Left(AppError.FileMissing(absent.toString)))
  }
  check("замена файла каталогом и обратно сохраняет восстанавливаемые снимки") {
    val directory = Files.createDirectory(root.resolve("file-directory-source"))
    val item = directory.resolve("item")
    put(item, "original file")
    var changed = value(Container.create(root.resolve("file-directory.store"), "File-directory", false, None,
      List(Source(directory.toString, true, Nil))))
    changed = backup(changed)._1

    Files.delete(item)
    put(item.resolve("child.txt"), "nested file")
    val directoryPlan = scan(changed)
    assert(directoryPlan.report.deleted.map(_.rel) == List("s0/item"))
    changed = value(Container.backup(changed, None, cfg, directoryPlan, None))._1
    val directoryDest = Files.createDirectory(root.resolve("restore-directory"))
    val restoredDirectory = value(Container.restore(changed, None, 2, directoryDest))
    assert(restoredDirectory.problems.isEmpty && restoredDirectory.restored == 1)
    assert(text(directoryDest.resolve("s0/item/child.txt")) == "nested file")

    Files.delete(item.resolve("child.txt"))
    Files.delete(item)
    put(item, "file again")
    val filePlan = scan(changed)
    assert(filePlan.report.deleted.map(_.rel) == List("s0/item/child.txt"))
    changed = value(Container.backup(changed, None, cfg, filePlan, None))._1
    val fileDest = Files.createDirectory(root.resolve("restore-file-again"))
    val restoredFile = value(Container.restore(changed, None, 3, fileDest))
    assert(restoredFile.problems.isEmpty && restoredFile.restored == 1)
    assert(text(fileDest.resolve("s0/item")) == "file again")
    val oldDest = Files.createDirectory(root.resolve("restore-before-replacement"))
    value(Container.restore(changed, None, 1, oldDest))
    assert(text(oldDest.resolve("s0/item")) == "original file")
  }
  check("каталог самого источника можно заменить одиночным файлом") {
    val directory = Files.createDirectory(root.resolve("replaced-source"))
    put(directory.resolve("old.txt"), "old")
    val empty = value(Container.create(root.resolve("replaced-source.store"), "Replaced source", false, None,
      List(Source(directory.toString, true, Nil))))
    val first = backup(empty)._1
    Files.delete(directory.resolve("old.txt"))
    Files.delete(directory)
    put(directory, "new root file")
    val plan = scan(first)
    assert(plan.report.deleted.map(_.rel) == List("s0/old.txt"))
    val next = value(Container.backup(first, None, cfg, plan, None))._1
    val dest = Files.createDirectory(root.resolve("restore-replaced-source"))
    val restored = value(Container.restore(next, None, 2, dest))
    assert(restored.problems.isEmpty && restored.restored == 1)
    assert(text(dest.resolve("s0/replaced-source")) == "new root file")
  }
  check("отключение источника после scan не превращается в удаление файлов") {
    val directory = Files.createDirectory(root.resolve("disconnect-source"))
    put(directory.resolve("old.txt"), "keep backup")
    val empty = value(Container.create(root.resolve("disconnect.store"), "Disconnect", false, None,
      List(Source(directory.toString, true, Nil))))
    val first = backup(empty)._1
    Files.delete(directory.resolve("old.txt"))
    val plan = scan(first)
    assert(plan.report.deleted.nonEmpty)
    Files.delete(directory)
    val (next, result) = value(Container.backup(first, None, cfg, plan, None))
    assert(result.deleted == 0 && result.problems.exists(_.isInstanceOf[AppError.SourceMissing]))
    assert(next.lastVersion.flatMap(_.snapshot.get("s0/old.txt")).exists(_.hash.nonEmpty))
  }
  check("успешная дата учитывается независимо для каждого источника") {
    val good = Files.createDirectory(root.resolve("good-source"))
    val changing = Files.createDirectory(root.resolve("changing-source"))
    put(good.resolve("good.txt"), "good")
    put(changing.resolve("changing.txt"), "before scan")
    val empty = value(Container.create(root.resolve("partial-sources.store"), "Partial sources", false, None,
      List(Source(good.toString, true, Nil), Source(changing.toString, true, Nil),
        Source(root.resolve("offline-source").toString, true, Nil))))
    val plan = scan(empty)
    put(changing.resolve("changing.txt"), "changed after scan")
    val (next, result) = value(Container.backup(empty, None, cfg, plan, None))
    assert(result.added == 1 && result.problems.size == 2)
    assert(next.sources.head.lastBackup.contains(result.version.timestamp))
    assert(next.sources.tail.forall(_.lastBackup.isEmpty))
    assert(value(Container.open(next.containerPath, None)).sources == next.sources)
  }
  check("небезопасный путь снимка не выходит за каталог восстановления") {
    val version = meta.lastVersion.get
    val ref = version.snapshot.values.find(_.hash.nonEmpty).get
    val unsafe = version.copy(snapshot = Map("s0/../../outside.txt" -> ref))
    val damaged = meta.copy(versions = List(unsafe))
    val dest = Files.createDirectory(root.resolve("restore-unsafe"))
    assert(Container.restoreConflicts(damaged, unsafe.number, dest).isLeft)
    val result = value(Container.restore(damaged, None, unsafe.number, dest))
    assert(result.restored == 0 && result.problems.nonEmpty)
    assert(!Files.exists(root.resolve("outside.txt")))
  }
  println(s"Проверок пройдено: $passed. Временные данные: $root")
