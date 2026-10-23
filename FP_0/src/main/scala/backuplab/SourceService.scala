// управляем источниками и получаем список файлов с учётом вложенных папок и исключений
// изменения списка сохраняем через ContainerService, а список файлов нужен ScanService

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

object SourceService {
  // добавляем источник; SourceService проверяет путь и сохраняет новый список через ContainerService
  def addSource(
      session: StorageSession,
      // путь, введённый пользователем
      pathText: String,
      // нужно ли обходить вложенные папки
      includeSubdirectories: Boolean,
      // список правил исключения
      exclusions: List[String]
  ): Either[String, BackupSource] = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      val path = Paths.get(pathText).toAbsolutePath.normalize()
      // обрабатываем отсутствие файла или папки
      if (!Files.exists(path)) {
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left("Указанный файл или каталог не существует.")
      }

      if (session.data.sources.exists(_.path.equalsIgnoreCase(path.toString))) {
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left("Этот источник уже добавлен.")
      }

      // путь к оригиналу либо описание источника
      val source = BackupSource(
        id = session.data.nextSourceId,
        path = path.toString,
        includeSubdirectories = includeSubdirectories,
        exclusions = exclusions,
        enabled = true,
        lastSuccessfulBackup = None
      )

      val newData = session.data.copy(
        sources = session.data.sources :+ source,
        nextSourceId = session.data.nextSourceId + 1
      )

      saveChange(session, newData).map(_ => source)
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(s"Не удалось добавить источник: ${error.getMessage}")
    }
  }

  // меняем путь, вложенные каталоги и исключения выбранного источника
  def editSource(
      session: StorageSession,
      // номер записи
      id: Int,
      // путь, введённый пользователем
      pathText: String,
      // нужно ли обходить вложенные папки
      includeSubdirectories: Boolean,
      // список правил исключения
      exclusions: List[String]
  ): Either[String, Unit] = {
    session.data.sources.find(_.id == id) match {
      // значения нет: например, ничего не найдено или ответ не получен
      case None => Left("Источник с таким номером не найден.")
      // значение есть; извлекаем его из Some и используем в этой ветке
      case Some(oldSource) =>
        // операции могут закончиться ошибкой; ниже catch обработает исключение
        try {
          val path = Paths.get(pathText).toAbsolutePath.normalize()
          val duplicate = session.data.sources.exists { source =>
            source.id != id && source.path.equalsIgnoreCase(path.toString)
          }

          if (duplicate) {
            Left("Источник с таким путём уже существует.")
          } else {
            val changedSource = oldSource.copy(
              path = path.toString,
              includeSubdirectories = includeSubdirectories,
              exclusions = exclusions
            )
            val sources = session.data.sources.map { source =>
              if (source.id == id) changedSource else source
            }
            saveChange(session, session.data.copy(sources = sources))
          }
        } catch {
          // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
          case NonFatal(error) => Left(s"Не удалось изменить источник: ${error.getMessage}")
        }
    }
  }

  // удаляем источник из списка проверки; сохранённая история его файлов остаётся
  def removeSource(session: StorageSession, id: Int): Either[String, Unit] = {
    if (!session.data.sources.exists(_.id == id)) {
      Left("Источник с таким номером не найден.")
    } else {
      val sources = session.data.sources.filterNot(_.id == id)
      saveChange(session, session.data.copy(sources = sources))
    }
  }

  // переключаем enabled: отключённый источник временно не участвует в проверке
  def toggleSource(session: StorageSession, id: Int): Either[String, Boolean] = {
    session.data.sources.find(_.id == id) match {
      // значения нет: например, ничего не найдено или ответ не получен
      case None => Left("Источник с таким номером не найден.")
      // значение есть; извлекаем его из Some и используем в этой ветке
      case Some(found) =>
        // новое значение enabled: меняем true на false или наоборот
        val newState = !found.enabled
        val sources = session.data.sources.map { source =>
          if (source.id == id) source.copy(enabled = newState) else source
        }
        saveChange(session, session.data.copy(sources = sources)).map(_ => newState)
    }
  }

  // возвращаем доступен, недоступен или отключён для показа источника
  def state(source: BackupSource): String = {
    if (!source.enabled) {
      "отключён"
    } else {
      // операции могут закончиться ошибкой; ниже catch обработает исключение
      try {
        val path = Paths.get(source.path)
        // проверяем, существует ли указанный путь
        if (Files.exists(path) && Files.isReadable(path)) "доступен" else "недоступен"
      } catch {
        // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
        case NonFatal(_) => "недоступен"
      }
    }
  }

  // обходим источник с учётом вложенных каталогов и исключений
  // возвращаем список файлов и отдельный список ошибок обхода
  def collectFiles(source: BackupSource): Either[String, (List[Path], List[String])] = {
    // корень выбранного источника или диск
    val root = Paths.get(source.path)
    // обрабатываем отсутствие файла или папки
    if (!Files.exists(root) || !Files.isReadable(root)) {
      // прерываем этот метод и возвращаем ошибку вызывающему коду
      return Left(source.path)
    }

    if (Files.isRegularFile(root)) {
      // возвращаем успешный результат, дальнейшие строки метода не выполняются
      return Right((List(root), Nil))
    }

    if (!Files.isDirectory(root)) {
      // прерываем этот метод и возвращаем ошибку вызывающему коду
      return Left(source.path)
    }

    // получаем файлы для обработки
    val files = ListBuffer[Path]()
    // сюда собираем сообщения об ошибках
    val errors = ListBuffer[String]()
    // глубина обхода: 1 без вложенных папок либо все уровни
    val maximumDepth = if (source.includeSubdirectories) Int.MaxValue else 1

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      Files.walkFileTree(
        root,
        java.util.Collections.emptySet[java.nio.file.FileVisitOption](),
        maximumDepth,
        new SimpleFileVisitor[Path]() {
          // перед входом в папку проверяем исключения; ненужную папку пропускаем целиком
          override def preVisitDirectory(
              directory: Path,
              attributes: BasicFileAttributes
          ): FileVisitResult = {
            if (directory != root && isExcluded(root.relativize(directory), source.exclusions)) {
              // пропускаем папку вместе со всем её содержимым
              FileVisitResult.SKIP_SUBTREE
            } else {
              // продолжаем обход остальных файлов и папок
              FileVisitResult.CONTINUE
            }
          }

          // при обходе добавляем только обычные файлы, не подходящие под исключения
          override def visitFile(
              file: Path,
              attributes: BasicFileAttributes
          ): FileVisitResult = {
            // путь относительно корня источника или рабочей папки
            val relativePath = root.relativize(file)
            if (attributes.isRegularFile && !isExcluded(relativePath, source.exclusions)) {
              files += file
            }
            // продолжаем обход остальных файлов и папок
            FileVisitResult.CONTINUE
          }

          // запоминаем ошибку доступа и продолжаем обход остальных файлов
          override def visitFileFailed(file: Path, error: java.io.IOException): FileVisitResult = {
            errors += file.toString
            // продолжаем обход остальных файлов и папок
            FileVisitResult.CONTINUE
          }
        }
      )

      Right((files.toList, errors.toList))
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(_) => Left(source.path)
    }
  }

  // получаем путь внутри источника; для отдельного файла берём его имя
  def relativePath(source: BackupSource, file: Path): String = {
    // корень выбранного источника или диск
    val root = Paths.get(source.path)
    val relative = if (Files.isRegularFile(root)) root.getFileName else root.relativize(file)
    relative.toString.replace('\\', '/')
  }

  // проверяем имя и относительный путь по правилам исключения
  private def isExcluded(relativePath: Path, exclusions: List[String]): Boolean = {
    // части пути, которые будем проверять по исключениям
    val parts = relativePath.iterator()
    // имена отдельных частей пути
    val names = scala.collection.mutable.ListBuffer[String]()
    while (parts.hasNext) {
      names += parts.next().toString
    }

    // нормализованный относительный путь со слешами
    val fullPath = relativePath.toString.replace('\\', '/')
    exclusions.exists { rule =>
      // регулярное выражение для одного правила
      val regex = globToRegex(rule.toLowerCase)
      names.exists(_.toLowerCase.matches(regex)) || fullPath.toLowerCase.matches(regex)
    }
  }

  // переводим простые правила со звёздочкой и вопросительным знаком в регулярное выражение
  private def globToRegex(rule: String): String = {
    // результат обработки
    val result = new StringBuilder("^")

    // выполняем блок для каждого элемента коллекции
    rule.foreach {
      case '*' => result.append(".*")
      case '?' => result.append('.')
      case character if "\\.^$|()[]{}+".contains(character) =>
        result.append('\\').append(character)
      case character => result.append(character)
    }

    result.append('$').toString()
  }

  // временно устанавливаем новые данные и сохраняем контейнер
  // если запись не удалась, возвращаем старые данные сессии
  private def saveChange(session: StorageSession, newData: StorageData): Either[String, Unit] = {
    // запоминаем метаданные до изменения, чтобы вернуть их при ошибке
    val oldData = session.data
    // устанавливаем подготовленные метаданные перед сохранением
    session.data = newData

    ContainerService.save(session) match {
      // эта ветка выполняется если всё кул
      case Right(_) => Right(())
      // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
      case Left(error) =>
        // возвращаем прежнее состояние в памяти, поскольку запись не удалась
        session.data = oldData
        try StorageDataService.save(session.workDirectory, oldData)
        // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
        catch { case NonFatal(_) => () }
        // передаём сообщение об ошибке вызывающему коду
        Left(error)
    }
  }
}

