package backup

import java.nio.file.Path
import scala.util.control.NonFatal
import upickle.default.*

// Совместимое JSON-представление необязательных строк и пар переименования.
private[backup] object ModelJson:
  given ReadWriter[Option[String]] = readwriter[ujson.Value].bimap(
    _.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
    value => if value == ujson.Null then None else Some(value.str)
  )
  given ReadWriter[(String, String)] = readwriter[ujson.Value].bimap(
    pair => ujson.Obj("from" -> pair._1, "to" -> pair._2),
    value => (value("from").str, value("to").str)
  )

import ModelJson.given

// hash = None — маркер удаления в снимке.
final case class FileRef(hash: Option[String], size: Long) derives ReadWriter

final case class VersionInfo(
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
) derives ReadWriter:
  def summary: String =
    s"$number  $timestamp  +${added.size}  ~${modified.size}  -${deleted.size}  $result  ${comment.getOrElse("")}"

final case class StorageMeta(
    name: String,
    containerPath: Path,
    encrypted: Boolean,
    keyCheck: Option[String],
    created: String,
    sources: List[Source],
    versions: List[VersionInfo],
    fileVersions: Map[String, List[Int]],
    cipher: String = "AES/GCM/NoPadding"
):
  def lastVersion: Option[VersionInfo] = versions.maxByOption(_.number)

object StorageMeta:
  // Фактический путь контейнера исключён из сериализуемых метаданных.
  private case class Data(
      name: String,
      encrypted: Boolean,
      keyCheck: Option[String],
      created: String,
      sources: List[Source],
      versions: List[VersionInfo],
      fileVersions: Map[String, List[Int]],
      cipher: String = "AES/GCM/NoPadding"
  ) derives ReadWriter

  def toJson(meta: StorageMeta): ujson.Value =
    val value = writeJs(Data(meta.name, meta.encrypted, meta.keyCheck, meta.created,
      meta.sources, meta.versions, meta.fileVersions, meta.cipher))
    value("format") = 2
    value

  def fromJson(value: ujson.Value, path: Path): Either[AppError, StorageMeta] =
    try
      val format = value("format").num
      if format != 1 && format != 2 then Left(AppError.DamagedStorage("Неподдерживаемый формат"))
      else if format == 1 && value("encrypted").bool then
        Left(AppError.DamagedStorage("Старое шифрование: экспортируйте файлы через ЛР0 и создайте новое хранилище"))
      else
        val migrated = ujson.Obj.from(value.obj)
        if !migrated("encrypted").bool then migrated("keyCheck") = ujson.Null
        val data = read[Data](migrated)
        val badVersion = data.versions.exists(v => v.number < 1 || v.bytes < 0 ||
          v.snapshot.values.exists(f => f.size < 0 || f.hash.exists(_.isEmpty)))
        if data.name.trim.isEmpty || data.created.isEmpty || badVersion ||
            data.versions.map(_.number).distinct.size != data.versions.size ||
            (data.encrypted && (data.keyCheck.forall(_.isEmpty) || data.cipher != "AES/GCM/NoPadding")) then
          Left(AppError.DamagedStorage("Некорректные метаданные"))
        else Right(StorageMeta(data.name, path, data.encrypted, data.keyCheck, data.created,
          data.sources, data.versions, data.fileVersions, data.cipher))
    catch case NonFatal(_) => Left(AppError.DamagedStorage("Некорректный JSON хранилища"))

final case class WalkedFile(rel: String, size: Long, mtime: Long, hash: String)
final case class LastRecord(rel: String, hash: String, size: Long, srcIndex: Option[Int])

final case class ChangeReport(
    newFiles: List[WalkedFile],
    changed: List[(LastRecord, WalkedFile)],
    deleted: List[LastRecord],
    renamed: List[(LastRecord, WalkedFile)],
    unreadable: List[String],
    unavailableSources: List[String],
    scannedCount: Int
):
  def totalNewBytes: Long = newFiles.map(_.size).sum + changed.map(_._2.size).sum
  def isEmpty: Boolean = newFiles.isEmpty && changed.isEmpty && deleted.isEmpty &&
    renamed.isEmpty && unreadable.isEmpty && unavailableSources.isEmpty

final case class ScanPlan(
    report: ChangeReport,
    problems: List[AppError],
    scannedSources: Set[Int]
)
final case class BackupResult(
    version: VersionInfo,
    added: Int,
    updated: Int,
    deleted: Int,
    skipped: Int,
    bytes: Long,
    problems: List[AppError] = Nil
)
