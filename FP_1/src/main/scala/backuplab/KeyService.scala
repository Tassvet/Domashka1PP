package backuplab

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

object KeyService {
  def defaultKeyPath(config: AppConfig, containerPath: Path): Path = {
    Paths.get(config.keyDirectory).toAbsolutePath.normalize()
      .resolve(containerPath.getFileName.toString + ".key")
  }

  def saveKey(config: AppConfig, containerPath: Path, key: Array[Byte]): Either[AppError, Path] = {
    val path = defaultKeyPath(config, containerPath)
    FileHelper.attempt("сохранение ключа", path.toString) {
      Files.createDirectories(path.getParent)
      Files.writeString(path, CryptoService.keyToText(key), StandardCharsets.UTF_8)
      path
    }
  }

  def loadKey(path: Path): Either[AppError, Array[Byte]] = {
    if (!Files.isRegularFile(path)) Left(AppError.FileUnavailable(path.toString))
    else FileHelper.attempt("чтение ключа", path.toString) {
      Files.readString(path, StandardCharsets.UTF_8)
    }.flatMap(CryptoService.textToKey)
  }
}
