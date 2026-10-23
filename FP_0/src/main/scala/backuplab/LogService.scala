// добавляем события в журнал с датой, временем и уровнем сообщения
// если запись журнала не удалась, сообщаем об этом в консоли

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.util.control.NonFatal

class LogService(private var logPath: Path) {
  // меняем путь журнала после успешной перезагрузки конфигурации
  def updatePath(newPath: String): Unit = {
    logPath = Paths.get(newPath)
  }

  // записываем обычное событие с уровнем INFO через общий метод write
  def info(message: String): Unit = write("INFO", message)

  // записываем предупреждение с уровнем WARNING
  def warning(message: String): Unit = write("WARNING", message)

  // записываем ошибку с уровнем ERROR
  def error(message: String): Unit = write("ERROR", message)

  // добавляем строку в конец журнала; прежние записи сохраняются
  // при ошибке доступа к журналу выводим сообщение в консоль
  private def write(level: String, message: String): Unit = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      Option(logPath.toAbsolutePath.getParent).foreach(parent => Files.createDirectories(parent))
      // собираем строку журнала с датой и уровнем
      val line = s"${TimeHelper.now()} [$level] $message${System.lineSeparator()}"
      // записываем данные; режим создания или замены указан в следующих аргументах
      Files.writeString(
        logPath,
        line,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) =>
        System.err.println(s"Не удалось записать журнал: ${error.getMessage}")
    }
  }
}

