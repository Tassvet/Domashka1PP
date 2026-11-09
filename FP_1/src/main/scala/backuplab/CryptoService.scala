package backuplab

import java.security.SecureRandom
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import scala.util.Try

object CryptoService {
  private val random = new SecureRandom()

  def generateKey(): Array[Byte] = {
    val key = new Array[Byte](32)
    random.nextBytes(key)
    key
  }

  def generateIv(): Array[Byte] = {
    val iv = new Array[Byte](12)
    random.nextBytes(iv)
    iv
  }

  def keyToText(key: Array[Byte]): String = {
    Base64.getUrlEncoder.withoutPadding().encodeToString(key).grouped(4).mkString("-")
  }

  def textToKey(text: String): Either[AppError, Array[Byte]] = {
    val compact = text.filterNot(_.isWhitespace)
    // убираем только разделители групп, сам Base64URL тоже может содержать дефис
    val grouped = compact.length == 53 && (4 until 53 by 5).forall(compact(_) == '-')
    val encoded = if (grouped) compact.zipWithIndex.filter(_._2 % 5 != 4).map(_._1).mkString
                  else compact
    Try(Base64.getUrlDecoder.decode(encoded)).toEither.left.map(_ => AppError.InvalidKey)
      .flatMap { key =>
        if (key.length == 32) Right(key) else Left(AppError.InvalidKey)
      }
  }

  def encryptionCipher(key: Array[Byte], iv: Array[Byte]): Cipher = cipher(Cipher.ENCRYPT_MODE, key, iv)
  def decryptionCipher(key: Array[Byte], iv: Array[Byte]): Cipher = cipher(Cipher.DECRYPT_MODE, key, iv)

  // ошибки библиотеки перехватываются в ContainerService
  private def cipher(mode: Int, key: Array[Byte], iv: Array[Byte]): Cipher = {
    val result = Cipher.getInstance("AES/GCM/NoPadding")
    result.init(mode, new SecretKeySpec(key, "AES"), new GCMParameterSpec(128, iv))
    result
  }
}
