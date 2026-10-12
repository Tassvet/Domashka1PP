// используем готовый алгоритм AES-GCM из Java для шифрования и расшифровки
// ключ — 32 случайных байта; iv — случайное значение для отдельного шифрования

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.security.SecureRandom
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import scala.util.control.NonFatal

object CryptoService {
  // генератор случайных байтов для ключа и iv
  private val random = new SecureRandom()

  // создаём 32 случайных байта для ключа AES-256
  def generateKey(): Array[Byte] = {
    // создаём или получаем ключ шифрования
    val key = new Array[Byte](32)
    // заполняем массив ключа случайными байтами
    random.nextBytes(key)
    // возвращаем созданный массив ключа
    key
  }

  // создаём 12 случайных байт для нового шифрования контейнера
  def generateIv(): Array[Byte] = {
    // создаём или читаем iv для операции шифрования
    val iv = new Array[Byte](12)
    // заполняем массив iv случайными байтами
    random.nextBytes(iv)
    // возвращаем массив iv
    iv
  }

  // превращаем байты ключа в текст Base64 и группируем символы для показа
  def keyToText(key: Array[Byte]): String = {
    // получаем текст для дальнейшей обработки
    val text = Base64.getUrlEncoder.withoutPadding().encodeToString(key)
    text.grouped(4).mkString("-")
  }

  // преобразуем текст обратно в 32 байта; при неверном формате возвращаем Left
  def textToKey(text: String): Either[String, Array[Byte]] = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // убираем разделители перед разбором ключа
      val cleanText = text.replace("-", "").replace(" ", "")
      // создаём или получаем ключ шифрования
      val key = Base64.getUrlDecoder.decode(cleanText)

      // для выбранного AES-256 требуется ровно 32 байта ключа
      if (key.length != 32) {
        Left("Ключ имеет неправильную длину.")
      } else {
        Right(key)
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(_) => Left("Ключ имеет неправильный формат.")
    }
  }

  // настраиваем готовый Cipher на шифрование с переданным ключом и iv
  def encryptionCipher(key: Array[Byte], iv: Array[Byte]): Cipher = {
    // готовый объект Java, выполняющий AES-GCM
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    cipher.init(
      Cipher.ENCRYPT_MODE,
      new SecretKeySpec(key, "AES"),
      new GCMParameterSpec(128, iv)
    )
    // возвращаем настроенный объект шифрования
    cipher
  }

  // настраиваем Cipher на расшифровку; нужен тот же ключ и iv из заголовка
  def decryptionCipher(key: Array[Byte], iv: Array[Byte]): Cipher = {
    // готовый объект Java, выполняющий AES-GCM
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    cipher.init(
      Cipher.DECRYPT_MODE,
      new SecretKeySpec(key, "AES"),
      new GCMParameterSpec(128, iv)
    )
    // возвращаем настроенный объект шифрования
    cipher
  }
}

