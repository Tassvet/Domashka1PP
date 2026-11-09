
package backuplab

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.util.control.NonFatal

class LogService(private var logPath: Path) {
  var lastError: Option[AppError] = None

  def updatePath(newPath: String): Unit = {
    logPath = Paths.get(newPath)
  }

  def info(message: String): Unit = write("INFO", message)

  def warning(message: String): Unit = write("WARNING", message)

  def error(message: String): Unit = write("ERROR", message)

  private def write(level: String, message: String): Unit = {
    try {
      Option(logPath.toAbsolutePath.getParent).foreach(parent => Files.createDirectories(parent))
      val line = s"${TimeHelper.now()} [$level] $message${System.lineSeparator()}"
      Files.writeString(
        logPath,
        line,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    } catch {
      case NonFatal(error) =>
        lastError = Some(AppError.IoFailure("запись журнала", logPath.toString, error.getClass.getSimpleName))
    }
  }
}

