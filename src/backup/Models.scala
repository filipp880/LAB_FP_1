package backup

import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

object ModelJson:
  def optionalText(value: Option[String]): ujson.Value = value match
    case Some(text) => ujson.Str(text)
    case None => ujson.Null

  def readOptionalText(value: ujson.Value): Option[String] =
    if value == ujson.Null then None else Some(value.str)

// hash = None — маркер удаления в снимке.
case class FileRef(hash: Option[String], size: Long)

case class VersionInfo(
    number: Int,
    timestamp: String,
    comment: Option[String],
    result: String,
    added: List[String],
    modified: List[String],
    deleted: List[String],
    renamed: List[(String, String)],
    bytes: Long,
    snapshot: Map[String, FileRef]
):
  def summary: String =
    s"$number  $timestamp  +${added.size}  ~${modified.size}  -${deleted.size}  $result  ${comment.getOrElse("")}"

case class StorageMeta(
    name: String,
    containerPath: Path,
    encrypted: Boolean,
    keyCheck: Option[String],
    created: String,
    sources: List[Source],
    versions: List[VersionInfo],
    cipher: String = "AES/GCM/NoPadding"
):
  def lastVersion: Option[VersionInfo] =
    var latest: Option[VersionInfo] = None
    for version <- versions do
      latest match
        case None => latest = Some(version)
        case Some(saved) =>
          if version.number > saved.number then latest = Some(version)
    latest

object StorageMeta:
  private def versionToJson(version: VersionInfo): ujson.Value =
    val snapshot = ujson.Obj()
    for (path, file) <- version.snapshot do
      val record = ujson.Obj()
      record("hash") = ModelJson.optionalText(file.hash)
      record("size") = ujson.Num(file.size.toDouble)
      snapshot(path) = record
    val renamed = version.renamed.map { case (from, to) =>
      val pair = ujson.Obj()
      pair("from") = from
      pair("to") = to
      pair
    }
    val json = ujson.Obj()
    json("number") = version.number
    json("timestamp") = version.timestamp
    json("comment") = ModelJson.optionalText(version.comment)
    json("result") = version.result
    json("added") = ujson.Arr.from(version.added)
    json("modified") = ujson.Arr.from(version.modified)
    json("deleted") = ujson.Arr.from(version.deleted)
    json("renamed") = ujson.Arr.from(renamed)
    json("bytes") = ujson.Num(version.bytes.toDouble)
    json("snapshot") = snapshot
    json

  private def versionFromJson(value: ujson.Value): VersionInfo =
    var snapshot = Map.empty[String, FileRef]
    for (path, file) <- value("snapshot").obj do
      val hash = ModelJson.readOptionalText(file("hash"))
      val size = file("size").num.toLong
      snapshot = snapshot.updated(path, FileRef(hash, size))
    val renamedValues = value("renamed").arr.toList
    val renamed = renamedValues.map { pair =>
      (pair("from").str, pair("to").str)
    }
    val addedValues = value("added").arr.toList
    val added = addedValues.map(item => item.str)
    val modifiedValues = value("modified").arr.toList
    val modified = modifiedValues.map(item => item.str)
    val deletedValues = value("deleted").arr.toList
    val deleted = deletedValues.map(item => item.str)
    val number = value("number").num.toInt
    val timestamp = value("timestamp").str
    val comment = ModelJson.readOptionalText(value("comment"))
    val result = value("result").str
    val bytes = value("bytes").num.toLong
    VersionInfo(
      number = number,
      timestamp = timestamp,
      comment = comment,
      result = result,
      added = added,
      modified = modified,
      deleted = deleted,
      renamed = renamed,
      bytes = bytes,
      snapshot = snapshot
    )

  def toJson(meta: StorageMeta): ujson.Value =
    // Путь файла не сохраняем: контейнер можно перенести в другую папку.
    val sources = meta.sources.map(source => Source.toJson(source))
    val versions = meta.versions.map(version => versionToJson(version))
    val json = ujson.Obj()
    json("format") = 2
    json("name") = meta.name
    json("encrypted") = meta.encrypted
    json("keyCheck") = ModelJson.optionalText(meta.keyCheck)
    json("created") = meta.created
    json("sources") = ujson.Arr.from(sources)
    json("versions") = ujson.Arr.from(versions)
    json("cipher") = meta.cipher
    json

  // Читаем и проверяем метаданные единственного поддерживаемого формата.
  def fromJson(value: ujson.Value, path: Path): Either[AppError, StorageMeta] =
    try
      val format = value("format").num
      if format != 2 then Left(AppError.DamagedStorage("Поддерживается только формат 2"))
      else
        val name = value("name").str
        val created = value("created").str
        val encrypted = value("encrypted").bool
        val keyCheck = if encrypted then ModelJson.readOptionalText(value("keyCheck")) else None
        val sourceValues = value("sources").arr.toList
        val sources = sourceValues.map(source => Source.fromJson(source))
        val versionValues = value("versions").arr.toList
        val versions = versionValues.map(version => versionFromJson(version))
        val cipher = value("cipher").str
        var invalidMetadata = false
        if name.trim.isEmpty then invalidMetadata = true
        if created.isEmpty then invalidMetadata = true
        val seenNumbers = ArrayBuffer.empty[Int]
        for version <- versions do
          if version.number < 1 then invalidMetadata = true
          if version.bytes < 0 then invalidMetadata = true
          if seenNumbers.contains(version.number) then invalidMetadata = true
          seenNumbers += version.number
          for (_, file) <- version.snapshot do
            val emptyHash = file.hash.exists(hash => hash.isEmpty)
            if file.size < 0 then invalidMetadata = true
            if emptyHash then invalidMetadata = true
        if encrypted then
          val hasKey = keyCheck.exists(key => key.nonEmpty)
          if !hasKey then invalidMetadata = true
          if cipher != "AES/GCM/NoPadding" then invalidMetadata = true
        if invalidMetadata then
          Left(AppError.DamagedStorage("Некорректные метаданные"))
        else
          val meta = StorageMeta(
            name = name,
            containerPath = path,
            encrypted = encrypted,
            keyCheck = keyCheck,
            created = created,
            sources = sources,
            versions = versions,
            cipher = cipher
          )
          Right(meta)
    catch case NonFatal(_) => Left(AppError.DamagedStorage("Некорректный JSON хранилища"))

case class WalkedFile(rel: String, size: Long, mtime: Long, hash: String)
case class LastRecord(rel: String, hash: String, size: Long, srcIndex: Option[Int])

case class ChangeReport(
    newFiles: List[WalkedFile],
    changed: List[(LastRecord, WalkedFile)],
    deleted: List[LastRecord],
    renamed: List[(LastRecord, WalkedFile)],
    unreadable: List[String],
    unavailableSources: List[String],
    scannedCount: Int
):
  def totalNewBytes: Long =
    val newBytes = newFiles.map(file => file.size).sum
    val modifiedBytes = changed.map { case (_, file) => file.size }.sum
    newBytes + modifiedBytes
  def isEmpty: Boolean =
    if newFiles.nonEmpty then return false
    if changed.nonEmpty then return false
    if deleted.nonEmpty then return false
    if renamed.nonEmpty then return false
    if unreadable.nonEmpty then return false
    if unavailableSources.nonEmpty then return false
    true

case class ScanPlan(
    report: ChangeReport,
    problems: List[AppError],
    scannedSources: List[Int]
)
case class BackupResult(
    version: VersionInfo,
    added: Int,
    updated: Int,
    deleted: Int,
    skipped: Int,
    bytes: Long,
    problems: List[AppError] = Nil
)
