// сохраняем найденные изменения и создаём новую версию хранилища
// ScanService даёт список изменений, HashService проверяет данные, ContainerService записывает контейнер

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

object BackupService {
  // создаём резервную версию из изменений
  // проверяем файлы перед копированием; успешные изменения передаём ContainerService для записи
  def createBackup(
      session: StorageSession,
      // результат предыдущей проверки файлов
      scanResult: ScanResult,
      // необязательный комментарий пользователя
      comment: String,
      // настройки программы
      config: AppConfig,
      // журнал для записи событий
      log: LogService
  ): Either[String, BackupResult] = {
    // без изменений не создаём новую резервную версию
    if (scanResult.changes.isEmpty) {
      // прерываем этот метод и возвращаем ошибку вызывающему коду
      return Left("Изменений для сохранения нет.")
    }

    checkFreeSpace(session, scanResult) match {
      // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
      case Left(error) =>
        log.warning("Резервное копирование не начато: недостаточно места или устройство недоступно")
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left(error)
      // эта ветка выполняется если всё кул
      case Right(_)    => ()
    }

    log.info("Начато резервное копирование")

    // номер создаваемой резервной версии
    // берём свободный номер для новой резервной версии
    val versionId = session.data.nextVersionId
    // сюда собираем только успешно подготовленные записи новой версии
    val newRevisions = ListBuffer[FileRevision]()
    // новые файлы или счётчик новых файлов
    var added = 0
    // счётчик изменённых файлов
    var modified = 0
    // удалённые файлы или счётчик удалений
    var deleted = 0
    // счётчик перемещений
    var moved = 0
    // счётчик пропущенных файлов
    var skipped = 0
    // счётчик байтов новых блоков данных
    var writtenBytes = 0L

    scanResult.changes.foreach { change =>
      change.changeType match {
        // для удаления создаём отметку, байты нового файла не копируем
        case "DELETED" =>
          // добавляем успешно подготовленную запись в историю новой версии
          newRevisions += deletedRevision(change, versionId, change.relativePath)
          // увеличиваем соответствующий счётчик на один
          deleted += 1

        // при перемещении сохраняем новый путь и отметку удаления старого
        case "MOVED" =>
          saveFileRevision(session, change, versionId, "ADDED") match {
            // эта ветка выполняется если всё кул
            case Right((revision, bytes)) =>
              // добавляем успешно подготовленную запись в историю новой версии
              newRevisions += revision
              // добавляем успешно подготовленную запись в историю новой версии
              newRevisions += deletedRevision(
                change,
                versionId,
                change.previousRelativePath.getOrElse(change.relativePath)
              )
              // увеличиваем соответствующий счётчик на один
              moved += 1
              // прибавляем объём новых записанных данных
              writtenBytes += bytes
            // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
            case Left(_) => skipped += 1
          }

        // одна ветка обрабатывает новые и изменённые файлы
        case "ADDED" | "MODIFIED" =>
          saveFileRevision(session, change, versionId, change.changeType) match {
            // эта ветка выполняется если всё кул
            case Right((revision, bytes)) =>
              // добавляем успешно подготовленную запись в историю новой версии
              newRevisions += revision
              // прибавляем объём новых записанных данных
              writtenBytes += bytes
              if (change.changeType == "ADDED") added += 1 else modified += 1
            // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
            case Left(_) => skipped += 1
          }

        case _ => skipped += 1
      }
    }

    // если сохранить не удалось ни одного изменения, новую версию не создаём
    if (newRevisions.isEmpty) {
      log.warning("Резервная копия не создана: все файлы пропущены")
      // прерываем этот метод и возвращаем ошибку вызывающему коду
      return Left("Ни одно изменение не удалось сохранить.")
    }

    // обозначаем полный либо частичный успех
    val resultText = if (skipped == 0) "успешно" else "частично"
    // номер формата либо сведения о создаваемой резервной версии
    val version = BackupVersion(
      versionId,
      TimeHelper.now(),
      added,
      modified,
      deleted,
      moved,
      skipped,
      writtenBytes,
      resultText,
      comment
    )

    // номера источников с успешно подготовленными записями
    val successfulSourceIds = newRevisions.map(_.sourceId).toSet
    // обновляем даты для источников
    val updatedSources = session.data.sources.map { source =>
      if (successfulSourceIds.contains(source.id)) {
        source.copy(lastSuccessfulBackup = Some(version.createdAt))
      } else {
        source
      }
    }

    // старая история вместе с записями новой версии
    val allRevisions = session.data.revisions ++ newRevisions.toList
    // история после применения ограничения версий
    val keptRevisions = keepRecentRevisions(allRevisions, config.maximumVersionsPerFile)
    // запоминаем метаданные до изменения, чтобы вернуть их при ошибке
    val oldData = session.data

    session.data = session.data.copy(
      sources = updatedSources,
      versions = session.data.versions :+ version,
      revisions = keptRevisions,
      nextVersionId = versionId + 1
    )

    ContainerService.save(session) match {
      // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
      case Left(error) =>
        // возвращаем прежнее состояние в памяти, поскольку запись не удалась
        session.data = oldData
        try StorageDataService.save(session.workDirectory, oldData)
        // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
        catch { case NonFatal(_) => () }
        log.error("Не удалось завершить резервное копирование")
        // передаём сообщение об ошибке вызывающему коду
        Left(error)

      // эта ветка выполняется если всё кул
      case Right(_) =>
        removeUnusedDataFiles(session)
        ContainerService.save(session) match {
          // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
          case Left(_) => log.warning("Не удалось удалить неиспользуемые старые данные из контейнера")
          // эта ветка выполняется если всё кул
          case Right(_) => ()
        }

        log.info(
          s"Резервное копирование завершено, создана версия $versionId, " +
            s"успешно обработано ${added + modified + deleted + moved}, пропущено $skipped"
        )
        Right(BackupResult(versionId, added, modified, deleted, moved, skipped, writtenBytes))
    }
  }

  // проверяем, что файл не изменился после scan, и сохраняем его данные
  // после копирования проверяем размер, время и хеш; при изменении возвращаем ошибку
  private def saveFileRevision(
      session: StorageSession,
      change: FileChange,
      // номер версии хранилища
      versionId: Int,
      // тип события: добавление, изменение, удаление или перемещение
      changeType: String
  ): Either[String, (FileRevision, Long)] = {
    // временный файл для безопасной записи
    var temporaryFile: Path = null

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // путь к оригиналу либо описание источника
      val source = Paths.get(change.absolutePath)
      // проверяем, что по пути есть обычный файл
      if (!Files.isRegularFile(source) || !Files.isReadable(source)) {
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left("Файл недоступен")
      }

      // свойства оригинала до копирования
      val attributesBefore = Files.readAttributes(
        source,
        classOf[java.nio.file.attribute.BasicFileAttributes]
      )

      // заново проверяем содержимое после scan
      val currentHash = HashService.calculate(source, session.data.hashAlgorithm)
      if (attributesBefore.size() != change.size || currentHash != change.hash) {
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left("Файл изменился после проверки")
      }

      // имя блока строится из хеша; одинаковые данные могут использовать один блок
      val relativeDataFile = s"data/${change.hash}.bin"
      // путь к сохранённым байтам в рабочей папке
      val storedFile = session.workDirectory.resolve(relativeDataFile)
      // число байтов нового блока; при повторном использовании равно нулю
      var copiedBytes = 0L

      // проверяем, существует ли уже правильный блок с этим хешем
      val storedFileIsValid = Files.isRegularFile(storedFile) &&
        HashService.calculate(storedFile, session.data.hashAlgorithm) == change.hash

      if (!storedFileIsValid) {
        // удаляем указанный файл, если он существует
        Files.deleteIfExists(storedFile)
        // создаём каталог и отсутствующие родительские папки
        Files.createDirectories(storedFile.getParent)
        temporaryFile = storedFile.resolveSibling(s"${storedFile.getFileName}.${UUID.randomUUID()}.part")
        // копируем байты в файл или поток назначения
        Files.copy(source, temporaryFile, StandardCopyOption.REPLACE_EXISTING)

        // хеш только что скопированных данных
        val copiedHash = HashService.calculate(temporaryFile, session.data.hashAlgorithm)
        // свойства оригинала после копирования
        val attributesAfter = Files.readAttributes(
          source,
          classOf[java.nio.file.attribute.BasicFileAttributes]
        )

        // проверяем, не поменялся ли оригинал в процессе копирования
        val changedDuringCopy =
          attributesBefore.size() != attributesAfter.size() ||
            attributesBefore.lastModifiedTime() != attributesAfter.lastModifiedTime() ||
            copiedHash != change.hash

        if (changedDuringCopy) {
          // удаляем указанный файл, если он существует
          Files.deleteIfExists(temporaryFile)
          // прерываем этот метод и возвращаем ошибку вызывающему коду
          return Left("Файл изменился во время копирования")
        }

        // перемещаем подготовленный файл на целевой путь
        Files.move(temporaryFile, storedFile, StandardCopyOption.REPLACE_EXISTING)
        copiedBytes = change.size
      }

      // запись состояния файла для истории
      val revision = FileRevision(
        change.sourceId,
        change.relativePath,
        versionId,
        changeType,
        change.size,
        change.modifiedTime,
        change.hash,
        relativeDataFile
      )

      Right((revision, copiedBytes))
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(error.getMessage)
    // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
    } finally {
      if (temporaryFile != null) {
        try Files.deleteIfExists(temporaryFile)
        // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
        catch { case NonFatal(_) => () }
      }
    }
  }

  // создаём запись об удалении без нового файла данных
  // такая запись нужна для восстановления состояния на выбранную дату
  private def deletedRevision(
      change: FileChange,
      // номер версии хранилища
      versionId: Int,
      // путь файла внутри источника
      relativePath: String
  ): FileRevision = {
    FileRevision(
      change.sourceId,
      relativePath,
      versionId,
      "DELETED",
      change.size,
      change.modifiedTime,
      change.hash,
      ""
    )
  }

  // оставляем ограниченное число последних записей для каждого пути
  // для удалённого файла дополнительно сохраняем последнее содержимое, чтобы оно не пропало сразу
  // после сокращения истории некоторые очень старые состояния нельзя восстановить полностью
  // для удаления оставляем ещё и последнюю запись содержимого: поэтому записей может быть больше лимита
  private def keepRecentRevisions(
      // история состояний отдельных файлов
      revisions: List[FileRevision],
      // ограничение истории одного файла
      maximumVersionsPerFile: Int
  ): List[FileRevision] = {
    revisions
      // объединяем записи в группы по указанному признаку
      .groupBy(revision => (revision.sourceId, revision.relativePath))
      .values
      // преобразуем элементы и объединяем полученные результаты в одну коллекцию
      .flatMap { fileRevisions =>
        // сортируем историю одного пути по номеру версии
        val sorted = fileRevisions.sortBy(_.versionId)
        // берём последние записи согласно ограничению
        val recent = sorted.takeRight(maximumVersionsPerFile)
        // последняя запись с содержимым, без отметки удаления
        val latestContent = sorted.filter(_.changeType != "DELETED").lastOption

        latestContent match {
          // значение есть; извлекаем его из Some и используем в этой ветке
          case Some(content)
              if !recent.contains(content) &&
                recent.lastOption.exists(_.changeType == "DELETED") =>
            content :: recent
          case _ => recent
        }
      }
      // собираем результат в список List
      .toList
  }

  // удаляем из рабочей папки блоки данных, на которые больше нет ссылок в истории
  private def removeUnusedDataFiles(session: StorageSession): Unit = {
    // имена блоков, на которые ещё ссылается история
    val usedFiles = session.data.revisions.map(_.dataFile).filter(_.nonEmpty).toSet
    // каталог с сохранёнными байтами
    val dataDirectory = session.workDirectory.resolve("data")

    if (Files.isDirectory(dataDirectory)) {
      // получаем файлы для обработки
      val files = Files.list(dataDirectory)
      // операции могут закончиться ошибкой; ниже catch обработает исключение
      try {
        // итератор по файлам или путям
        val iterator = files.iterator()
        while (iterator.hasNext) {
          val file = iterator.next()
          // имя блока в том же виде, что записан в метаданных
          val relativeName = s"data/${file.getFileName}"
          if (Files.isRegularFile(file) && !usedFiles.contains(relativeName)) {
            // удаляем указанный файл, если он существует
            Files.deleteIfExists(file)
          }
        }
      // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
      } finally {
        // закрываем поток и освобождаем связанный с ним ресурс
        files.close()
      }
    }
  }

  // оцениваем место для нового контейнера с изменениями и небольшим запасом
  private def checkFreeSpace(
      session: StorageSession,
      // результат предыдущей проверки файлов
      scanResult: ScanResult
  ): Either[String, Unit] = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // родительский каталог, в котором создаём или сохраняем файл
      val parent = session.containerPath.getParent
      // получаем сведения о файловой системе устройства
      val fileStore = Files.getFileStore(parent)
      // размер имеющегося контейнера
      val existingSize = if (Files.exists(session.containerPath)) Files.size(session.containerPath) else 0L
      // примерная оценка места для новой записи
      val requiredSpace = existingSize + scanResult.dataSize + 10L * 1024 * 1024

      if (fileStore.getUsableSpace < requiredSpace) {
        Left("На устройстве недостаточно свободного места.")
      } else {
        // Right сообщает об успехе; () — значение Unit, дополнительных данных нет
        Right(())
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(s"Не удалось проверить свободное место: ${error.getMessage}")
    }
  }
}

