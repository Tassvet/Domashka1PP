// работаем с одним файлом .store: создаём, открываем и сохраняем хранилище
// внутренние файлы упакованы в zip; для шифрования обращаемся к CryptoService
// пока сессия открыта, распакованные данные лежат во временной папке без шифрования
// close удаляет эту папку при обычном завершении; при аварийной остановке она может остаться

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import javax.crypto.{CipherInputStream, CipherOutputStream}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object ContainerService {
  // специальная отметка в начале файла: по ней узнаём свой контейнер
  private val magic = "BACKUPLAB1".getBytes(StandardCharsets.US_ASCII)
  // номер поддерживаемого формата .store
  private val formatVersion = 1

  // создаём временную папку и начальные метаданные, затем сохраняем контейнер через save
  // возвращаем Right с сессией либо Left с сообщением об ошибке
  def create(
      // путь к переносимому файлу .store
      containerPath: Path,
      // название нового хранилища
      storageName: String,
      // включено ли шифрование
      encrypted: Boolean,
      // ключ шифрования; None означает отсутствие ключа
      key: Option[Array[Byte]],
      // название алгоритма проверки содержимого
      hashAlgorithm: String
  ): Either[String, StorageSession] = {
    if (encrypted && key.isEmpty) {
      // прерываем этот метод и возвращаем ошибку вызывающему коду
      return Left("Для зашифрованного хранилища требуется ключ.")
    }

    // папка для работы с распакованными данными
    // временная папка с распакованными данными
    var workDirectory: Path = null

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      workDirectory = Files.createTempDirectory("backuplab-")
      // создаём каталог и отсутствующие родительские папки
      Files.createDirectories(workDirectory.resolve("data"))

      // сведения о хранилище
      val data = StorageData(
        name = storageName,
        createdAt = TimeHelper.now(),
        encrypted = encrypted,
        hashAlgorithm = hashAlgorithm,
        sources = Nil,
        versions = Nil,
        revisions = Nil,
        nextSourceId = 1,
        nextVersionId = 1
      )

      // сессия связывает контейнер, временную папку и метаданные
      val session = new StorageSession(
        containerPath.toAbsolutePath.normalize(),
        workDirectory,
        encrypted,
        key,
        data
      )

      save(session) match {
        // эта ветка выполняется если всё кул
        case Right(_)    => Right(session)
        // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
        case Left(error) =>
          close(session)
          // передаём сообщение об ошибке вызывающему коду
          Left(error)
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) =>
        if (workDirectory != null) {
          deleteDirectory(workDirectory)
        }
        Left(s"Не удалось создать хранилище: ${error.getMessage}")
    }
  }

  // читаем контейнер, извлекаем файлы и загружаем метаданные через StorageDataService
  // при успехе возвращаем сессию; при ошибке удаляем временную папку
  def open(containerPath: Path, key: Option[Array[Byte]]): Either[String, StorageSession] = {
    // делаем путь абсолютным и убираем лишние части
    val normalizedPath = containerPath.toAbsolutePath.normalize()

    // проверяем, что по пути есть обычный файл
    if (!Files.isRegularFile(normalizedPath)) {
      // прерываем этот метод и возвращаем ошибку вызывающему коду
      return Left("Файл хранилища не найден.")
    }

    // папка для работы с распакованными данными
    // временная папка с распакованными данными
    var workDirectory: Path = null

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      workDirectory = Files.createTempDirectory("backuplab-")
      // признак включённого шифрования
      val encrypted = extractContainer(normalizedPath, workDirectory, key)

      StorageDataService.load(workDirectory) match {
        // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
        case Left(error) =>
          deleteDirectory(workDirectory)
          // передаём сообщение об ошибке вызывающему коду
          Left(error)
        // эта ветка выполняется если всё кул
        case Right(data) =>
          if (data.encrypted != encrypted) {
            deleteDirectory(workDirectory)
            Left("Признак шифрования в контейнере повреждён.")
          } else {
            Right(new StorageSession(normalizedPath, workDirectory, encrypted, key, data))
          }
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) =>
        if (workDirectory != null) {
          deleteDirectory(workDirectory)
        }
        val message = if (key.nonEmpty) {
          "Не удалось открыть хранилище. Возможно, введён неправильный ключ или файл повреждён."
        } else {
          s"Не удалось открыть хранилище: ${error.getMessage}"
        }
        Left(message)
    }
  }

  // читаем заголовок без распаковки и узнаём, требуется ли ключ
  def isEncrypted(containerPath: Path): Either[String, Boolean] = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      val input = new DataInputStream(new BufferedInputStream(Files.newInputStream(containerPath)))
      // операции могут закончиться ошибкой; ниже catch обработает исключение
      try {
        readAndCheckHeader(input)
        Right(input.readBoolean())
      // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
      } finally {
        // закрываем поток и освобождаем связанный с ним ресурс
        input.close()
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(s"Не удалось прочитать заголовок хранилища: ${error.getMessage}")
    }
  }

  // сначала сохраняем метаданные, затем пишем новый контейнер во временный файл рядом со старым
  // после успешной записи заменяем исходный контейнер; при ошибке возвращаем Left
  def save(session: StorageSession): Either[String, Unit] = {
    // временный файл для безопасной записи
    var temporaryFile: Path = null

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // родительский каталог, в котором создаём или сохраняем файл
      val parent = session.containerPath.getParent
      if (parent == null || !Files.isDirectory(parent) || !Files.isWritable(parent)) {
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left("Каталог хранилища недоступен для записи.")
      }

      StorageDataService.save(session.workDirectory, session.data)
      temporaryFile = Files.createTempFile(parent, ".backuplab-", ".tmp")
      writeContainer(session, temporaryFile)

      // операции могут закончиться ошибкой; ниже catch обработает исключение
      try {
        // перемещаем подготовленный файл на целевой путь
        Files.move(
          temporaryFile,
          session.containerPath,
          StandardCopyOption.REPLACE_EXISTING,
          StandardCopyOption.ATOMIC_MOVE
        )
      } catch {
        case _: AtomicMoveNotSupportedException =>
          // перемещаем подготовленный файл на целевой путь
          Files.move(temporaryFile, session.containerPath, StandardCopyOption.REPLACE_EXISTING)
      }

      // Right сообщает об успехе; () — значение Unit, дополнительных данных нет
      Right(())
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(s"Не удалось записать хранилище: ${error.getMessage}")
    // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
    } finally {
      if (temporaryFile != null) {
        try Files.deleteIfExists(temporaryFile)
        // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
        catch { case NonFatal(_) => () }
      }
    }
  }

  // удаляем временную рабочую папку открытого хранилища
  def close(session: StorageSession): Unit = {
    deleteDirectory(session.workDirectory)
  }

  // пишем заголовок, при шифровании iv, затем zip с внутренними файлами
  // CipherOutputStream шифрует данные во время записи
  private def writeContainer(session: StorageSession, target: Path): Unit = {
    // поток записи контейнера; буфер уменьшает число обращений к диску
    val fileOutput = new DataOutputStream(
      new BufferedOutputStream(Files.newOutputStream(target))
    )

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // записываем отметку, по которой узнаём формат BackupLab
      fileOutput.write(magic)
      // записываем номер формата как целое число
      fileOutput.writeInt(formatVersion)
      // сохраняем признак шифрования в открытом заголовке
      fileOutput.writeBoolean(session.encrypted)

      // поток внутреннего содержимого: обычный либо с шифрованием
      val payload: OutputStream = if (session.encrypted) {
        // создаём или получаем ключ шифрования
        val key = session.key.getOrElse(throw new IllegalStateException("Ключ отсутствует"))
        // создаём или читаем iv для операции шифрования
        val iv = CryptoService.generateIv()
        // перед iv сохраняем его длину
        fileOutput.writeInt(iv.length)
        // записываем iv; для расшифровки он потребуется вместе с ключом
        fileOutput.write(iv)
        // передаём накопленные байты заголовка из буфера дальше в поток
        fileOutput.flush()
        new CipherOutputStream(fileOutput, CryptoService.encryptionCipher(key, iv))
      } else {
        fileOutput
      }

      writeZip(session.workDirectory, payload)
    // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
    } finally {
      // закрываем поток и освобождаем связанный с ним ресурс
      try fileOutput.close()
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      catch { case NonFatal(_) => () }
    }
  }

  // проверяем заголовок и извлекаем внутренние файлы
  // для зашифрованного контейнера поток сначала расшифровывается
  private def extractContainer(
      // путь к переносимому файлу .store
      containerPath: Path,
      // временная папка с распакованными данными
      workDirectory: Path,
      // ключ шифрования; None означает отсутствие ключа
      key: Option[Array[Byte]]
  ): Boolean = {
    // поток чтения контейнера
    val fileInput = new DataInputStream(
      new BufferedInputStream(Files.newInputStream(containerPath))
    )

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      readAndCheckHeader(fileInput)
      // признак включённого шифрования
      val encrypted = fileInput.readBoolean()

      // поток внутреннего содержимого: обычный либо с шифрованием
      val payload: InputStream = if (encrypted) {
        // получаем ключ; без него расшифровка невозможна
        val actualKey = key.getOrElse(throw new IllegalArgumentException("Требуется ключ доступа"))
        // размер iv из заголовка; ожидаем 12 байт
        val ivLength = fileInput.readInt()
        // другую длину iv считаем ошибкой формата
        if (ivLength != 12) {
          throw new IllegalArgumentException("Неправильный вектор шифрования")
        }
        // создаём или читаем iv для операции шифрования
        val iv = new Array[Byte](ivLength)
        // читаем iv из заголовка полностью
        fileInput.readFully(iv)
        new CipherInputStream(fileInput, CryptoService.decryptionCipher(actualKey, iv))
      } else {
        fileInput
      }

      extractZip(payload, workDirectory)
      encrypted
    // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
    } finally {
      // закрываем поток и освобождаем связанный с ним ресурс
      try fileInput.close()
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      catch { case NonFatal(_) => () }
    }
  }

  // проверяем специальную отметку BackupLab и номер формата контейнера
  private def readAndCheckHeader(input: DataInputStream): Unit = {
    // сюда читаем отметку формата
    val readMagic = new Array[Byte](magic.length)
    // читаем отметку целиком; слишком короткий файл вызовет ошибку
    input.readFully(readMagic)

    // сравниваем байты заголовка с ожидаемой отметкой
    if (!java.util.Arrays.equals(magic, readMagic)) {
      throw new IllegalArgumentException("это не файл BackupLab")
    }

    // номер формата либо сведения о создаваемой резервной версии
    val version = input.readInt()
    // открываем только поддерживаемую версию формата
    if (version != formatVersion) {
      throw new IllegalArgumentException(s"версия формата $version не поддерживается")
    }
  }

  // упаковываем обычные файлы рабочей папки в zip, сохраняя относительные пути
  private def writeZip(directory: Path, output: OutputStream): Unit = {
    // поток для упаковки или чтения zip
    val zip = new ZipOutputStream(new BufferedOutputStream(output))

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // получаем файлы для обработки
      val files = Files.walk(directory)
      // операции могут закончиться ошибкой; ниже catch обработает исключение
      try {
        files.iterator().asScala
          // оставляем только элементы, подходящие под условие
          .filter(path => Files.isRegularFile(path))
          .foreach { path =>
            // путь относительно корня источника или рабочей папки
            val relativePath = directory.relativize(path).toString.replace('\\', '/')
            // начинаем очередной файл в архиве под относительным именем
            zip.putNextEntry(new ZipEntry(relativePath))
            // копируем байты в файл или поток назначения
            Files.copy(path, zip)
            // заканчиваем текущую запись архива
            zip.closeEntry()
          }
      // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
      } finally {
        // закрываем поток и освобождаем связанный с ним ресурс
        files.close()
      }
    // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
    } finally {
      // закрываем поток и освобождаем связанный с ним ресурс
      zip.close()
    }
  }

  // извлекаем записи zip; запрещаем пути, выходящие за рабочую папку
  private def extractZip(input: InputStream, directory: Path): Unit = {
    // поток для упаковки или чтения zip
    val zip = new ZipInputStream(new BufferedInputStream(input))

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // очередная запись архива
      var entry = zip.getNextEntry

      // обрабатываем записи zip, пока в архиве ещё есть следующая запись
      while (entry != null) {
        // путь целевого файла
        val target = directory.resolve(entry.getName).normalize()
        // не позволяем записи архива выйти за рабочую папку
        if (!target.startsWith(directory)) {
          throw new IllegalArgumentException("контейнер содержит небезопасный путь")
        }

        if (entry.isDirectory) {
          // создаём каталог и отсутствующие родительские папки
          Files.createDirectories(target)
        } else {
          Option(target.getParent).foreach(parent => Files.createDirectories(parent))
          // копируем байты в файл или поток назначения
          Files.copy(zip, target, StandardCopyOption.REPLACE_EXISTING)
        }

        // заканчиваем текущую запись архива
        zip.closeEntry()
        // переходим к следующей записи; в конце получится null
        entry = zip.getNextEntry
      }
    // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
    } finally {
      // закрываем поток и освобождаем связанный с ним ресурс
      zip.close()
    }
  }

  // удаляем временные файлы от вложенных к родительским
  // этот метод используется при закрытии сессии и не удаляет сам контейнер
  private def deleteDirectory(directory: Path): Unit = {
    if (directory == null || !Files.exists(directory)) {
      return
    }

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // поток путей для обхода
      val paths = Files.walk(directory)
      // операции могут закончиться ошибкой; ниже catch обработает исключение
      try {
        paths.iterator().asScala.toList.sortBy(_.getNameCount).reverse.foreach { path =>
          try Files.deleteIfExists(path)
          // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
          catch { case NonFatal(_) => () }
        }
      // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
      } finally {
        // закрываем поток и освобождаем связанный с ним ресурс
        paths.close()
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(_) => ()
    }
  }
}

