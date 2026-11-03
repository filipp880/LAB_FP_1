package backup

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import scala.util.control.NonFatal

object Crypto:
  def normalize(algo: String): Either[AppError, String] = algo.toUpperCase match
    case "SHA-256" => Right("SHA-256")
    case "MD5" => Right("MD5")
    case other => Left(AppError.UnsupportedHash(other))

  def hex(bytes: Array[Byte]): String =
    val parts = bytes.map { byte =>
      var number = byte.toInt
      // Byte имеет значения от -128 до 127; здесь нужен диапазон от 0 до 255.
      if number < 0 then number += 256
      val text = Integer.toHexString(number)
      if text.length == 1 then "0" + text else text
    }
    parts.mkString

  def hash(algo: String, bytes: Array[Byte]): Either[AppError, String] =
    normalize(algo).flatMap { name =>
      try
        val digest = MessageDigest.getInstance(name)
        val hashBytes = digest.digest(bytes)
        Right(hex(hashBytes))
      catch case NonFatal(_) => Left(AppError.HashFailed(name))
    }

  def hash(algo: String, path: Path): Either[AppError, String] =
    try
      val bytes = Files.readAllBytes(path)
      hash(algo, bytes)
    catch case NonFatal(_) => Left(AppError.ReadFailed(path.toString))

  def generateKey(): String =
    val bytes = new Array[Byte](32)
    new SecureRandom().nextBytes(bytes)
    Base64.getEncoder.encodeToString(bytes)

  // Проверочное значение ключа зависит от названия хранилища.
  def keyCheck(key: String, name: String): String =
    val text = key + ":" + name
    val bytes = text.getBytes(UTF_8)
    val digest = MessageDigest.getInstance("SHA-256")
    val hashBytes = digest.digest(bytes)
    hex(hashBytes)

  private def aesKey(key: String): SecretKeySpec =
    val digest = MessageDigest.getInstance("SHA-256")
    val keyBytes = digest.digest(key.getBytes(UTF_8))
    new SecretKeySpec(keyBytes, "AES")

  // AES — готовый алгоритм из библиотеки. Первые 12 байт нужны для расшифровки.
  def encrypt(key: String, bytes: Array[Byte]): Either[AppError, Array[Byte]] =
    try
      val nonce = new Array[Byte](12)
      new SecureRandom().nextBytes(nonce)
      val cipher = Cipher.getInstance("AES/GCM/NoPadding")
      cipher.init(Cipher.ENCRYPT_MODE, aesKey(key), new GCMParameterSpec(128, nonce))
      val encryptedBytes = cipher.doFinal(bytes)
      Right(nonce ++ encryptedBytes)
    catch case NonFatal(_) => Left(AppError.EncryptionFailed)

  def decrypt(key: String, bytes: Array[Byte]): Either[AppError, Array[Byte]] =
    if bytes.length < 28 then Left(AppError.DamagedStorage("Повреждён зашифрованный блок"))
    else
      try
        val nonce = bytes.take(12)
        val encryptedBytes = bytes.drop(12)
        val cipher = Cipher.getInstance("AES/GCM/NoPadding")
        cipher.init(Cipher.DECRYPT_MODE, aesKey(key), new GCMParameterSpec(128, nonce))
        Right(cipher.doFinal(encryptedBytes))
      catch case NonFatal(_) => Left(AppError.DamagedStorage("Нарушена целостность зашифрованного блока"))
