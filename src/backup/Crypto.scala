package backup

import java.io.InputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import scala.util.Using

object Crypto:
  def normalize(algo: String): Either[AppError, String] = algo.toUpperCase match
    case "SHA-256" => Right("SHA-256")
    case "MD5" => Right("MD5")
    case other => Left(AppError.UnsupportedHash(other))

  def hex(bytes: Array[Byte]): String = bytes.map(b => f"${b & 255}%02x").mkString

  def hash(algo: String, bytes: Array[Byte]): Either[AppError, String] =
    normalize(algo).flatMap(a => Boundary.attempt("Вычисление хеша") {
      hex(MessageDigest.getInstance(a).digest(bytes))
    })

  def hash(algo: String, in: InputStream): Either[AppError, String] =
    normalize(algo).flatMap(a => Boundary.attempt("Чтение файла") {
      val digest = MessageDigest.getInstance(a)
      val buffer = new Array[Byte](65536)
      var count = in.read(buffer)
      while count > 0 do
        digest.update(buffer, 0, count)
        count = in.read(buffer)
      hex(digest.digest())
    })

  def hash(algo: String, path: Path): Either[AppError, String] =
    Boundary.attempt("Чтение исходного файла") {
      Using.resource(Files.newInputStream(path))(hash(algo, _))
    }.flatMap(identity)

  def generateKey(): String =
    val bytes = new Array[Byte](32)
    new SecureRandom().nextBytes(bytes)
    Base64.getEncoder.encodeToString(bytes)

  // Проверочное значение ключа зависит от названия хранилища.
  def keyCheck(key: String, name: String): String =
    hex(MessageDigest.getInstance("SHA-256").digest(s"$key:$name".getBytes(UTF_8)))

  private def aesKey(key: String): SecretKeySpec =
    new SecretKeySpec(MessageDigest.getInstance("SHA-256").digest(key.getBytes(UTF_8)), "AES")

  // Формат блока: nonce (12 байт) || шифротекст || тег AES-GCM (16 байт).
  def encrypt(key: String, bytes: Array[Byte]): Either[AppError, Array[Byte]] =
    Boundary.attempt("Шифрование") {
      val nonce = new Array[Byte](12)
      new SecureRandom().nextBytes(nonce)
      val cipher = Cipher.getInstance("AES/GCM/NoPadding")
      cipher.init(Cipher.ENCRYPT_MODE, aesKey(key), new GCMParameterSpec(128, nonce))
      nonce ++ cipher.doFinal(bytes)
    }

  def decrypt(key: String, bytes: Array[Byte]): Either[AppError, Array[Byte]] =
    if bytes.length < 28 then Left(AppError.DamagedStorage("Повреждён зашифрованный блок"))
    else Boundary.attempt("Расшифровка") {
      val cipher = Cipher.getInstance("AES/GCM/NoPadding")
      cipher.init(Cipher.DECRYPT_MODE, aesKey(key), new GCMParameterSpec(128, bytes.take(12)))
      cipher.doFinal(bytes.drop(12))
    }.left.map(_ => AppError.DamagedStorage("Нарушена целостность зашифрованного блока"))
