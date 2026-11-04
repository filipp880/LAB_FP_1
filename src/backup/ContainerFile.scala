package backup

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{FileAlreadyExistsException, Files, NoSuchFileException, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipException, ZipFile, ZipOutputStream}
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

// Здесь только устройство ZIP-файла. Сравнение файлов и версии — в Container.
object ContainerFile:
  val MetaEntry = "meta/filerecords.json"
  val BlobPrefix = "blobs/"

  def entryBytes(zip: ZipFile, name: String): Either[AppError, Array[Byte]] =
    // getEntry — метод Java: он возвращает null, если записи нет.
    Option(zip.getEntry(name)) match
      case None => Left(AppError.DamagedStorage(s"Отсутствует запись $name"))
      case Some(entry) =>
        try
          val input = zip.getInputStream(entry)
          try Right(input.readAllBytes())
          finally input.close()
        catch case NonFatal(_) => Left(AppError.ReadFailed(name))

  def readMeta(path: Path): Either[AppError, StorageMeta] =
    try
      val zip = new ZipFile(path.toFile)
      try
        entryBytes(zip, MetaEntry).flatMap { bytes =>
          try StorageMeta.fromJson(ujson.read(new String(bytes, UTF_8)), path)
          catch case NonFatal(_) => Left(AppError.DamagedStorage("Неверный JSON метаданных"))
        }
      finally zip.close()
    catch
      case _: NoSuchFileException => Left(AppError.FileMissing(path.toString))
      case _: ZipException => Left(AppError.DamagedStorage("Неверный ZIP-контейнер"))
      case NonFatal(_) => Left(AppError.ReadFailed(path.toString))

  private def writeEntry(out: ZipOutputStream, name: String, bytes: Array[Byte]): Unit =
    out.putNextEntry(new ZipEntry(name))
    out.write(bytes)
    out.closeEntry()

  // Копируем только те старые блоки, на которые ещё ссылается история.
  private def copyOldBlocks(target: Path, out: ZipOutputStream,
                            wanted: List[String], extra: List[String]): Unit =
    val old = new ZipFile(target.toFile)
    try
      for hash <- wanted do
        if !extra.contains(hash) then
          val name = BlobPrefix + hash
          val entry = Option(old.getEntry(name))
          entry match
            case None => ()
            case Some(value) =>
              out.putNextEntry(new ZipEntry(name))
              val input = old.getInputStream(value)
              try input.transferTo(out)
              finally input.close()
              out.closeEntry()
    finally old.close()

  def write(meta: StorageMeta, blobs: Map[String, Array[Byte]],
            key: Option[String], replacing: Boolean): Either[AppError, Unit] =
    val wanted = ArrayBuffer.empty[String]
    // Получаем хеши всех блоков, которые нужны сохранённым версиям.
    for version <- meta.versions do
      for record <- version.snapshot.values do
        record.hash match
          case Some(hash) =>
            if !wanted.contains(hash) then wanted += hash
          case None => ()

    val extra = ArrayBuffer.empty[(String, Array[Byte])]
    var problem: Option[AppError] = None
    // Подготавливаем новые блоки. При ошибке шифрования файл не перезаписывается.
    for (hash, bytes) <- blobs do
      if problem.isEmpty && wanted.contains(hash) then
        val encoded = key match
          case Some(value) => Crypto.encrypt(value, bytes)
          case None => Right(bytes)
        encoded match
          case Left(error) => problem = Some(error)
          case Right(data) => extra += ((hash, data))
    problem match
      case Some(error) => Left(error)
      case None => writePrepared(meta, wanted.toList, extra.toList, replacing)

  private def writePrepared(meta: StorageMeta, wanted: List[String],
                            extra: List[(String, Array[Byte])], replacing: Boolean): Either[AppError, Unit] =
    try
      val target = meta.containerPath.toAbsolutePath
      val temp = Files.createTempFile(target.getParent, "backup-", ".tmp")
      try
        val out = new ZipOutputStream(Files.newOutputStream(temp))
        try
          val extraHashes = extra.map { case (hash, _) => hash }
          if replacing then copyOldBlocks(target, out, wanted, extraHashes)
          val sortedBlocks = extra.sortBy { case (hash, _) => hash }
          for (hash, bytes) <- sortedBlocks do
            writeEntry(out, BlobPrefix + hash, bytes)
          val json = ujson.write(StorageMeta.toJson(meta), indent = 2)
          writeEntry(out, MetaEntry, json.getBytes(UTF_8))
        finally out.close()
        // Готовый ZIP заменяет старый только после успешной записи и закрытия.
        if replacing then Files.move(temp, target, StandardCopyOption.REPLACE_EXISTING)
        else Files.move(temp, target)
        Right(())
      finally Files.deleteIfExists(temp)
    catch
      case _: FileAlreadyExistsException => Left(AppError.FileExists(meta.containerPath.toString))
      case _: NoSuchFileException => Left(AppError.FileMissing(meta.containerPath.toString))
      case NonFatal(_) => Left(AppError.WriteFailed(meta.containerPath.toString))
