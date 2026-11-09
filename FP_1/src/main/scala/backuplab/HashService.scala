package backuplab

import java.io.BufferedInputStream
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.util.Using

object HashService {
  def calculate(path: Path, algorithm: String): Either[AppError, String] = {
    FileHelper.attempt("вычисление хеша", path.toString) {
      val digest = MessageDigest.getInstance(algorithm)
      val buffer = new Array[Byte](8192)
      Using.resource(new BufferedInputStream(Files.newInputStream(path))) { input =>
        var bytesRead = input.read(buffer)
        // -1 — обозначение конца потока в Java, а не ошибка приложения
        while (bytesRead != -1) {
          digest.update(buffer, 0, bytesRead)
          bytesRead = input.read(buffer)
        }
      }
      digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
    }
  }
}
