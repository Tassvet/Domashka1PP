// вычисляем хеш файла: это строка для сравнения содержимого и проверки повреждений
// читаем файл небольшими частями, чтобы не загружать его целиком в память

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.io.BufferedInputStream
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.util.Using

object HashService {
  // читаем файл блоками, передаём байты MessageDigest и возвращаем хеш в виде строки
  def calculate(path: Path, algorithm: String): String = {
    // MessageDigest постепенно получает байты и вычисляет хеш
    val digest = MessageDigest.getInstance(algorithm)
    // буфер на 8192 байта: читаем данные по частям
    val buffer = new Array[Byte](8192)

    // Using закроет открытый ресурс, когда работа с ним закончится
    Using.resource(new BufferedInputStream(Files.newInputStream(path))) { input =>
      // количество байтов в очередном блоке; -1 означает конец файла
      var bytesRead = input.read(buffer)

      // читаем следующие блоки, пока не дошли до конца файла
      while (bytesRead != -1) {
        // добавляем к хешу только фактически прочитанные байты из буфера
        digest.update(buffer, 0, bytesRead)
        // читаем следующий блок и обновляем число прочитанных байтов
        bytesRead = input.read(buffer)
      }
    }

    // получаем байты хеша и записываем каждый как два шестнадцатеричных символа
    digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
  }
}

