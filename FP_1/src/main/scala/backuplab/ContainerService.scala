package backuplab

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import javax.crypto.{CipherInputStream, CipherOutputStream}
import scala.jdk.CollectionConverters.*

object ContainerService {
  private val magic = "BACKUPLAB1".getBytes(StandardCharsets.US_ASCII)
  private val formatVersion = 1

  def create(path: Path, name: String, encrypted: Boolean, key: Option[Array[Byte]],
             hashAlgorithm: String): Either[AppError, StorageSession] = {
    if (encrypted && key.isEmpty) return Left(AppError.KeyRequired)
    if (encrypted && key.exists(_.length != 32)) return Left(AppError.InvalidKey)
    if (!Set("SHA-256", "SHA-512").contains(hashAlgorithm))
      return Left(AppError.InvalidSetting("hashAlgorithm", hashAlgorithm))
    FileHelper.attempt("создание рабочей папки", path.toString) {
      Files.createTempDirectory("backuplab-")
    }.flatMap { directory =>
      val data = StorageData(name, TimeHelper.now(), encrypted, hashAlgorithm, Nil, Nil, Nil, 1, 1)
      val session = new StorageSession(path.toAbsolutePath.normalize(), directory, encrypted, key, data)
      save(session).map(_ => session).left.map { error =>
        close(session)
        error
      }
    }
  }

  def open(path: Path, key: Option[Array[Byte]]): Either[AppError, StorageSession] = {
    if (!Files.isRegularFile(path)) return Left(AppError.ContainerNotFound(path.toString))
    FileHelper.attempt("создание рабочей папки", path.toString) {
      Files.createTempDirectory("backuplab-")
    }.flatMap { directory =>
      val result = for {
        encrypted <- extractContainer(path, directory, key)
        data <- StorageDataService.load(directory)
        _ <- if (data.encrypted == encrypted) Right(()) else Left(AppError.CorruptMetadata("шифрование"))
      } yield new StorageSession(path.toAbsolutePath.normalize(), directory, encrypted, key, data)
      result.left.map { error =>
        FileHelper.deleteDirectory(directory)
        error
      }
    }
  }

  def isEncrypted(path: Path): Either[AppError, Boolean] = {
    if (!Files.isRegularFile(path)) Left(AppError.ContainerNotFound(path.toString))
    else FileHelper.attempt("открытие заголовка", path.toString) {
      new DataInputStream(new BufferedInputStream(Files.newInputStream(path)))
    }.flatMap { input =>
      try readHeader(input)
      finally input.close()
    }
  }

  def save(session: StorageSession): Either[AppError, Unit] = {
    val parent = session.containerPath.getParent
    if (!Files.isDirectory(parent) || !Files.isWritable(parent))
      return Left(AppError.DestinationUnavailable(parent.toString))
    for {
      _ <- StorageDataService.save(session.workDirectory, session.data)
      temporary <- FileHelper.attempt("подготовка записи", parent.toString) {
        Files.createTempFile(parent, ".backuplab-", ".tmp")
      }
      _ <- try {
        writeContainer(session, temporary).flatMap { _ =>
          FileHelper.attempt("замена контейнера", session.containerPath.toString) {
            try Files.move(temporary, session.containerPath,
              StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
            catch {
              case _: AtomicMoveNotSupportedException =>
                Files.move(temporary, session.containerPath, StandardCopyOption.REPLACE_EXISTING)
            }
            ()
          }
        }
      } finally {
        FileHelper.attempt("удаление временного контейнера", temporary.toString)(Files.deleteIfExists(temporary))
      }
    } yield ()
  }

  def close(session: StorageSession): Unit = FileHelper.deleteDirectory(session.workDirectory)

  private def writeContainer(session: StorageSession, target: Path): Either[AppError, Unit] = {
    FileHelper.attempt("открытие контейнера для записи", target.toString) {
      new DataOutputStream(new BufferedOutputStream(Files.newOutputStream(target)))
    }.flatMap { output =>
      try {
        val payload = FileHelper.attempt("запись заголовка", target.toString) {
          output.write(magic)
          output.writeInt(formatVersion)
          output.writeBoolean(session.encrypted)
        }.flatMap { _ =>
          if (!session.encrypted) Right(output: OutputStream)
          else session.key.toRight(AppError.KeyRequired).flatMap { key =>
            FileHelper.attempt("подготовка шифрования", target.toString) {
              val iv = CryptoService.generateIv()
              output.writeInt(iv.length)
              output.write(iv)
              output.flush()
              new CipherOutputStream(output, CryptoService.encryptionCipher(key, iv)): OutputStream
            }
          }
        }
        payload.flatMap(stream => writeZip(session.workDirectory, stream))
      } finally output.close()
    }
  }

  private def extractContainer(path: Path, directory: Path, key: Option[Array[Byte]]): Either[AppError, Boolean] = {
    FileHelper.attempt("открытие контейнера", path.toString) {
      new DataInputStream(new BufferedInputStream(Files.newInputStream(path)))
    }.flatMap { input =>
      try {
        readHeader(input).flatMap { encrypted =>
          val extracted = if (!encrypted) extractZip(input, directory)
          else key.toRight(AppError.KeyRequired).flatMap { actualKey =>
            if (actualKey.length != 32) Left(AppError.InvalidKey)
            else decryptZip(input, directory, actualKey)
          }
          extracted.map(_ => encrypted)
        }
      } finally input.close()
    }
  }

  private def decryptZip(input: DataInputStream, directory: Path, key: Array[Byte]): Either[AppError, Unit] = {
    for {
      length <- FileHelper.attempt("чтение IV", "контейнер")(input.readInt())
      _ <- if (length == 12) Right(()) else Left(AppError.InvalidContainer)
      iv <- FileHelper.attempt("чтение IV", "контейнер") {
        val bytes = new Array[Byte](12)
        input.readFully(bytes)
        bytes
      }
      temporary <- FileHelper.attempt("подготовка расшифровки", "контейнер") {
        Files.createTempFile("backuplab-", ".zip")
      }
      _ <- try {
        // читаем шифрованный поток до конца, чтобы GCM обязательно проверил подлинность
        val decrypted = FileHelper.attempt("расшифровка", "контейнер") {
          val cipherInput = new CipherInputStream(input, CryptoService.decryptionCipher(key, iv))
          try Files.copy(cipherInput, temporary, StandardCopyOption.REPLACE_EXISTING)
          finally cipherInput.close()
        }.left.map(_ => AppError.CannotDecrypt)
        decrypted.flatMap { _ =>
          FileHelper.attempt("открытие ZIP", "контейнер")(Files.newInputStream(temporary)).flatMap { zip =>
            try extractZip(zip, directory)
            finally zip.close()
          }
        }
      } finally {
        FileHelper.attempt("удаление временного ZIP", temporary.toString)(Files.deleteIfExists(temporary))
      }
    } yield ()
  }

  private def readHeader(input: DataInputStream): Either[AppError, Boolean] = {
    FileHelper.attempt("чтение заголовка", "контейнер") {
      val bytes = new Array[Byte](magic.length)
      input.readFully(bytes)
      (bytes, input.readInt(), input.readBoolean())
    }.left.map(_ => AppError.InvalidContainer).flatMap { case (bytes, version, encrypted) =>
      if (!java.util.Arrays.equals(bytes, magic)) Left(AppError.InvalidContainer)
      else if (version != formatVersion) Left(AppError.UnsupportedFormat(version))
      else Right(encrypted)
    }
  }

  private def writeZip(directory: Path, output: OutputStream): Either[AppError, Unit] = {
    FileHelper.attempt("упаковка ZIP", directory.toString) {
      val zip = new ZipOutputStream(new BufferedOutputStream(output))
      try {
        val files = Files.walk(directory)
        try files.iterator().asScala.filter(Files.isRegularFile(_)).foreach { path =>
          zip.putNextEntry(new ZipEntry(directory.relativize(path).toString.replace('\\', '/')))
          Files.copy(path, zip)
          zip.closeEntry()
        }
        finally files.close()
      } finally zip.close()
    }
  }

  private def extractZip(input: InputStream, directory: Path): Either[AppError, Unit] = {
    FileHelper.attempt("открытие ZIP", "контейнер") {
      new ZipInputStream(new BufferedInputStream(input))
    }.flatMap { zip =>
      try {
        var result: Either[AppError, Unit] = Right(())
        var finished = false
        while (!finished && result.isRight) {
          val entry = FileHelper.attempt("чтение ZIP", "контейнер")(Option(zip.getNextEntry))
          result = entry.flatMap {
            case None =>
              finished = true
              Right(())
            case Some(item) =>
              FileHelper.safePath(directory, item.getName).flatMap { target =>
                FileHelper.attempt("распаковка ZIP", item.getName) {
                  if (item.isDirectory) Files.createDirectories(target)
                  else {
                    Option(target.getParent).foreach(Files.createDirectories(_))
                    Files.copy(zip, target, StandardCopyOption.REPLACE_EXISTING)
                  }
                  zip.closeEntry()
                }
              }
          }
        }
        result
      } finally zip.close()
    }
  }
}
