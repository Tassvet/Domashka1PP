// сравниваем исходные файлы с последним сохранённым состоянием по хешам
// проверка возвращает ScanResult и сама не добавляет резервную версию

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

object ScanService {
  // получаем файлы через SourceService и сравниваем хеши через HashService
  // недоступные источники не считаем удалёнными; возвращаем ScanResult для меню и backup
  def scan(
      session: StorageSession,
      // настройки программы
      config: AppConfig,
      // журнал для записи событий
      log: LogService
  ): ScanResult = {
    log.info("Начата проверка изменений")

    // сюда собираем обнаруженные изменения
    val changes = ListBuffer[FileChange]()
    // сюда записываем недоступные источники
    val unavailableSources = ListBuffer[String]()
    // сюда записываем ошибки чтения отдельных файлов
    val unreadableFiles = ListBuffer[String]()
    // для каждого пути берём последнее сохранённое состояние
    val latest = latestRevisions(session.data.revisions)

    session.data.sources.filter(_.enabled).foreach { source =>
      SourceService.collectFiles(source) match {
        // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
        case Left(path) =>
          unavailableSources += path

        // эта ветка выполняется если всё кул
        case Right((files, collectionErrors)) =>
          unreadableFiles ++= collectionErrors
          // пути найденных файлов; это помогает определить удаления
          val seenPaths = scala.collection.mutable.Set[String]()

          files.foreach { file =>
            // путь относительно корня источника или рабочей папки
            val relativePath = SourceService.relativePath(source, file)
            seenPaths += relativePath

            // операции могут закончиться ошибкой; ниже catch обработает исключение
            try {
              // свойства файла перед чтением
              val before = Files.readAttributes(file, classOf[java.nio.file.attribute.BasicFileAttributes])
              val hash = HashService.calculate(file, session.data.hashAlgorithm)
              // свойства файла после чтения
              val after = Files.readAttributes(file, classOf[java.nio.file.attribute.BasicFileAttributes])

              if (before.size() != after.size() || before.lastModifiedTime() != after.lastModifiedTime()) {
                unreadableFiles += s"${file.toString} (изменился во время проверки)"
              } else {
                // создаём или получаем ключ шифрования
                val key = (source.id, relativePath)
                latest.get(key) match {
                  // значения нет: например, ничего не найдено или ответ не получен
                  case None =>
                    changes += FileChange(
                      "ADDED",
                      source.id,
                      relativePath,
                      file.toAbsolutePath.toString,
                      after.size(),
                      after.lastModifiedTime().toMillis,
                      hash,
                      None
                    )
                  // значение есть; извлекаем его из Some и используем в этой ветке
                  // файл после сохранённого удаления снова появился: считаем его новым
                  case Some(previous) if previous.changeType == "DELETED" =>
                    changes += FileChange(
                      "ADDED",
                      source.id,
                      relativePath,
                      file.toAbsolutePath.toString,
                      after.size(),
                      after.lastModifiedTime().toMillis,
                      hash,
                      None
                    )
                  // значение есть; извлекаем его из Some и используем в этой ветке
                  // по этому пути уже есть копия, но содержимое теперь отличается
                  case Some(previous) if previous.hash != hash =>
                    changes += FileChange(
                      "MODIFIED",
                      source.id,
                      relativePath,
                      file.toAbsolutePath.toString,
                      after.size(),
                      after.lastModifiedTime().toMillis,
                      hash,
                      None
                    )
                  // остальные варианты не требуют действий
                  case _ => ()
                }
              }
            } catch {
              // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
              case NonFatal(_) => unreadableFiles += file.toString
            }
          }

          // удаления отмечаем только после полного обхода; ошибки папок могут скрыть существующие файлы
          if (collectionErrors.isEmpty) {
            latest.values
              // оставляем только элементы, подходящие под условие
              .filter(revision => revision.sourceId == source.id && revision.changeType != "DELETED")
              // убираем элементы, для которых условие истинно
              .filterNot(revision => seenPaths.contains(revision.relativePath))
              .foreach { revision =>
                changes += FileChange(
                  "DELETED",
                  source.id,
                  revision.relativePath,
                  "",
                  revision.size,
                  revision.modifiedTime,
                  revision.hash,
                  None
                )
              }
          }
      }
    }

    // результат обработки
    val result = detectMoves(changes.toList)
    // итог проверки: изменения и ошибки
    val scanResult = ScanResult(result, unavailableSources.toList, unreadableFiles.distinct.toList)

    log.info(
      s"Проверка завершена: новых ${scanResult.addedCount}, " +
        s"изменённых ${scanResult.modifiedCount}, удалённых ${scanResult.deletedCount}, " +
        s"перемещённых ${scanResult.movedCount}, недоступных источников " +
        s"${scanResult.unavailableSources.size}, непрочитанных файлов ${scanResult.unreadableFiles.size}"
    )

    scanResult
  }

  // группируем историю по источнику и пути и выбираем запись с наибольшим номером версии
  def latestRevisions(revisions: List[FileRevision]): Map[(Int, String), FileRevision] = {
    revisions
      // объединяем записи в группы по указанному признаку
      .groupBy(revision => (revision.sourceId, revision.relativePath))
      .view
      // преобразуем значения групп; сами ключи групп остаются прежними
      .mapValues(_.maxBy(_.versionId))
      // собираем пары ключ-значение в Map для поиска по ключу
      .toMap
  }

  // сопоставляем новые и исчезнувшие файлы по хешу и размеру внутри одного источника
  // совпадение считаем предполагаемым перемещением; одинаковое содержимое может быть неоднозначным
  private def detectMoves(changes: List[FileChange]): List[FileChange] = {
    // новые файлы или счётчик новых файлов
    val added = changes.filter(_.changeType == "ADDED")
    // удалённые файлы или счётчик удалений
    val deleted = changes.filter(_.changeType == "DELETED")
    // исчезнувшие файлы, уже сопоставленные с перемещением
    val usedDeleted = scala.collection.mutable.Set[(Int, String)]()
    // новые пути, которые уже отнесены к перемещениям
    val movedFromAdded = scala.collection.mutable.Set[(Int, String)]()
    // список предполагаемых перемещений
    val movedChanges = ListBuffer[FileChange]()

    added.foreach { newFile =>
      deleted.find { oldFile =>
        oldFile.sourceId == newFile.sourceId &&
        oldFile.hash == newFile.hash &&
        oldFile.size == newFile.size &&
        !usedDeleted.contains((oldFile.sourceId, oldFile.relativePath))
      }.foreach { oldFile =>
        usedDeleted += ((oldFile.sourceId, oldFile.relativePath))
        movedFromAdded += ((newFile.sourceId, newFile.relativePath))
        movedChanges += newFile.copy(
          changeType = "MOVED",
          previousRelativePath = Some(oldFile.relativePath)
        )
      }
    }

    // оставляем изменения, не заменённые событием MOVED
    val unchangedChanges = changes.filter {
      case change if change.changeType == "ADDED" =>
        !movedFromAdded.contains((change.sourceId, change.relativePath))
      case change if change.changeType == "DELETED" =>
        !usedDeleted.contains((change.sourceId, change.relativePath))
      case _ => true
    }

    unchangedChanges ++ movedChanges.toList
  }
}

