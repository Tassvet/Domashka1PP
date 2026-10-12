// восстанавливаем файлы выбранной версии в каталог назначения
// сначала проверяем сохранённые данные по хешу, затем копируем их и учитываем перезапись

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.UUID
import scala.util.control.NonFatal

object RestoreService {
  // для каждого пути выбираем последнее состояние не позднее нужной версии
  // пропускаем удалённые записи и восстанавливаем остальные с учётом подтверждения перезаписи
  def restore(
      session: StorageSession,
      // номер версии хранилища
      versionId: Int,
      // каталог, куда восстанавливаем файлы
      destination: Path,
      // настройки программы
      config: AppConfig,
      // функция, которая спрашивает разрешение на замену файла
      askOverwrite: Path => Boolean,
      // журнал для записи событий
      log: LogService
  ): Either[String, RestoreResult] = {
    if (!session.data.versions.exists(_.id == versionId)) {
      // прерываем этот метод и возвращаем ошибку вызывающему коду
      return Left("Версия с таким номером не найдена.")
    }

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // создаём каталог и отсутствующие родительские папки
      Files.createDirectories(destination)
      if (!Files.isDirectory(destination) || !Files.isWritable(destination)) {
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left("Каталог восстановления недоступен.")
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => return Left(s"Каталог восстановления недоступен: ${error.getMessage}")
    }

    log.info(s"Начато восстановление версии $versionId")

    // состояние файлов на выбранную версию
    val state = session.data.revisions
      // оставляем только элементы, подходящие под условие
      .filter(_.versionId <= versionId)
      // объединяем записи в группы по указанному признаку
      .groupBy(revision => (revision.sourceId, revision.relativePath))
      .values
      // преобразуем каждый элемент в новое значение; _ обозначает текущий элемент
      .map(_.maxBy(_.versionId))
      // оставляем только элементы, подходящие под условие
      .filter(_.changeType != "DELETED")
      // собираем результат в список List
      .toList
      .sortBy(revision => (revision.sourceId, revision.relativePath))

    // счётчик восстановленных файлов
    var restored = 0
    // счётчик пропущенных файлов
    var skipped = 0
    // счётчик ошибок восстановления
    var failed = 0

    state.foreach { revision =>
      // отдельная папка назначения для этого источника
      val sourceFolder = sourceFolderName(session, revision.sourceId)
      // полный путь этой папки
      val sourceDestination = destination.resolve(sourceFolder).normalize()
      // путь целевого файла
      val target = sourceDestination.resolve(revision.relativePath).normalize()

      if (!target.startsWith(sourceDestination)) {
        // увеличиваем соответствующий счётчик на один
        failed += 1
      } else {
        // решаем, можно ли записать файл с учётом настроек и ответа пользователя
        val canWrite = if (!Files.exists(target)) {
          true
        } else if (config.confirmBeforeRestoreOverwrite) {
          askOverwrite(target)
        } else {
          // при отключённом вопросе настройка определяет замену либо пропуск
          config.overwriteExistingWithoutConfirmation
        }

        if (!canWrite) {
          // увеличиваем соответствующий счётчик на один
          skipped += 1
        } else {
          restoreOneFile(session, revision, target, session.data.hashAlgorithm) match {
            // эта ветка выполняется если всё кул
            case Right(_) => restored += 1
            // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
            case Left(_)  => failed += 1
          }
        }
      }
    }

    log.info(
      s"Восстановление версии $versionId завершено: восстановлено $restored, " +
        s"пропущено $skipped, ошибок $failed"
    )
    Right(RestoreResult(restored, skipped, failed))
  }

  // проверяем хеш данных, копируем во временный файл и только затем заменяем целевой
  // после копирования снова проверяем хеш и возвращаем время изменения
  private def restoreOneFile(
      session: StorageSession,
      // одна запись истории файла
      revision: FileRevision,
      // путь, по которому нужно записать файл
      target: Path,
      // название алгоритма проверки содержимого
      hashAlgorithm: String
  ): Either[String, Unit] = {
    // временный файл для безопасной записи
    var temporaryFile: Path = null

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // путь к сохранённым байтам в рабочей папке
      val storedFile = session.workDirectory.resolve(revision.dataFile).normalize()
      if (!storedFile.startsWith(session.workDirectory) || !Files.isRegularFile(storedFile)) {
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left("Сохранённые данные отсутствуют")
      }

      if (HashService.calculate(storedFile, hashAlgorithm) != revision.hash) {
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left("Сохранённые данные повреждены")
      }

      Option(target.getParent).foreach(parent => Files.createDirectories(parent))
      temporaryFile = target.resolveSibling(s"${target.getFileName}.${UUID.randomUUID()}.part")
      // копируем байты в файл или поток назначения
      Files.copy(storedFile, temporaryFile, StandardCopyOption.REPLACE_EXISTING)

      if (HashService.calculate(temporaryFile, hashAlgorithm) != revision.hash) {
        // удаляем указанный файл, если он существует
        Files.deleteIfExists(temporaryFile)
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left("Ошибка проверки восстановленного файла")
      }

      // перемещаем подготовленный файл на целевой путь
      Files.move(temporaryFile, target, StandardCopyOption.REPLACE_EXISTING)
      Files.setLastModifiedTime(target, FileTime.fromMillis(revision.modifiedTime))
      // Right сообщает об успехе; () — значение Unit, дополнительных данных нет
      Right(())
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

  // делаем отдельное имя папки назначения из номера и имени источника
  // номер помогает разделить файлы разных источников
  private def sourceFolderName(session: StorageSession, sourceId: Int): String = {
    // имя источника для папки восстановления
    val originalName = session.data.sources
      .find(_.id == sourceId)
      // преобразуем элементы и объединяем полученные результаты в одну коллекцию
      .flatMap { source =>
        try Option(java.nio.file.Paths.get(source.path).getFileName).map(_.toString)
        // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
        catch { case NonFatal(_) => None }
      }
      // оставляем только элементы, подходящие под условие
      .filter(_.nonEmpty)
      .getOrElse("source")

    // заменяем неудобные символы имени на подчёркивания
    val safeName = originalName.replaceAll("[^a-zA-Zа-яА-Я0-9._-]", "_")
    s"${sourceId}_$safeName"
  }
}

