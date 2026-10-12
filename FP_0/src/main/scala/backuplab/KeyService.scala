// сохраняем ключ в отдельном файле и читаем его при открытии контейнера
// сюда обращается ConsoleApp только при выборе сохранения ключа в файл

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.util.control.NonFatal

object KeyService {
  // строим путь к файлу ключа в каталоге из конфигурации
  def defaultKeyPath(config: AppConfig, containerPath: Path): Path = {
    // имя файла без каталога
    val fileName = containerPath.getFileName.toString + ".key"
    Paths.get(config.keyDirectory).toAbsolutePath.normalize().resolve(fileName)
  }

  // записываем текст ключа отдельно от контейнера; возвращаем путь или сообщение об ошибке
  def saveKey(
      // настройки программы
      config: AppConfig,
      // путь к переносимому файлу .store
      containerPath: Path,
      // ключ шифрования; None означает отсутствие ключа
      key: Array[Byte]
  ): Either[String, Path] = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // путь к отдельному файлу ключа
      val keyPath = defaultKeyPath(config, containerPath)
      // создаём каталог и отсутствующие родительские папки
      Files.createDirectories(keyPath.getParent)
      // записываем данные; режим создания или замены указан в следующих аргументах
      Files.writeString(
        keyPath,
        CryptoService.keyToText(key),
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      Right(keyPath)
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(s"Не удалось сохранить файл ключа: ${error.getMessage}")
    }
  }

  // читаем файл ключа и проверяем его через CryptoService.textToKey
  def loadKey(path: Path): Either[String, Array[Byte]] = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // проверяем, что по пути есть обычный файл
      if (!Files.isRegularFile(path)) {
        Left("Файл ключа не найден.")
      } else {
        // получаем текст для дальнейшей обработки
        val text = Files.readString(path, StandardCharsets.UTF_8).trim
        CryptoService.textToKey(text)
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(s"Не удалось прочитать файл ключа: ${error.getMessage}")
    }
  }
}

