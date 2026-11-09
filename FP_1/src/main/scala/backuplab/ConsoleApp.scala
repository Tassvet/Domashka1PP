package backuplab

import java.nio.file.{FileSystems, Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

// только этот класс и InputHelper ведут диалог с пользователем
class ConsoleApp(configPath: Path, initialConfig: AppConfig, log: LogService) {
  private var config = initialConfig
  private var currentStorage: Option[StorageSession] = None
  private var running = true

  def run(): Unit = {
    println("Переносное резервное хранилище — ФП1")
    println("Введите help для просмотра команд.")
    log.info("Приложение запущено")
    try {
      while (running) {
        val result = try {
          InputHelper.read("\n> ").flatMap(Command.parse).flatMap(execute)
        } catch {
          case NonFatal(error) =>
            Left(AppError.IoFailure("выполнение команды", "приложение", error.getClass.getSimpleName))
        }
        result match {
          case Left(AppError.InputClosed) => running = false
          case Left(error) =>
            println(ErrorMessages.text(error))
            log.warning("Команда не выполнена")
          case Right(_) => ()
        }
        log.lastError.foreach(error => println(ErrorMessages.text(error)))
        log.lastError = None
      }
    } finally {
      currentStorage.foreach(ContainerService.close)
      log.info("Приложение завершено")
      println("Работа завершена.")
    }
  }

  private def execute(command: Command): Either[AppError, Unit] = command match {
    case Command.Create => createStorage()
    case Command.Open => openStorage()
    case Command.Info => withStorage(session => Right(showInfo(session)))
    case Command.Sources => withStorage(manageSources)
    case Command.Scan => withStorage(session => Right(scanStorage(session)))
    case Command.Backup => withStorage(createBackup)
    case Command.Versions => withStorage(session => Right(showVersions(session)))
    case Command.Restore => withStorage(restoreFiles)
    case Command.Verify => withStorage(session => Right(verifyStorage(session)))
    case Command.Reload => reloadConfig()
    case Command.Help => Right(showHelp())
    case Command.Exit =>
      running = false
      Right(())
    case Command.Empty => Right(())
  }

  private def showHelp(): Unit = println(
    """create   — создать хранилище
      |open     — открыть хранилище
      |info     — сведения о хранилище
      |sources  — настройка источников
      |scan     — поиск изменений
      |backup   — сохранить изменения
      |versions — список версий
      |restore  — восстановить версию
      |verify   — проверить целостность
      |reload   — перечитать config.properties
      |help     — список команд
      |exit     — выход""".stripMargin)

  private case class KeyChoice(key: Option[Array[Byte]], saveToFile: Boolean)

  private def createStorage(): Either[AppError, Unit] = {
    for {
      directory <- chooseDirectory(allowCreate = true)
      name <- InputHelper.readWithDefault("Имя файла хранилища", config.defaultContainerName)
      _ <- validateFileName(name)
      path = directory.resolve(name)
      _ <- if (!Files.exists(path)) Right(()) else confirm("Файл уже существует. Перезаписать?", false)
      title <- InputHelper.readWithDefault("Название хранилища", "Моя резервная копия")
      encrypted <- InputHelper.readYesNo("Использовать шифрование?", false)
      choice <- chooseKey(encrypted)
      session <- ContainerService.create(path, title, encrypted, choice.key, config.hashAlgorithm)
      _ = replaceCurrentStorage(session)
      _ = println(s"Хранилище создано: $path")
      _ = log.info(s"Создано хранилище: $path")
      _ = if (choice.saveToFile) choice.key.foreach(key => saveKey(path, key))
      _ <- addInitialSources(session)
    } yield ()
  }

  private def chooseKey(encrypted: Boolean): Either[AppError, KeyChoice] = {
    if (!encrypted) Right(KeyChoice(None, false))
    else {
      val key = CryptoService.generateKey()
      println("1. Сохранить ключ в каталоге из конфигурации")
      println("2. Показать ключ и не сохранять его")
      InputHelper.readInt("Способ хранения ключа: ", 1, 2).flatMap {
        case 1 => Right(KeyChoice(Some(key), true))
        case _ =>
          println("Ключ доступа:")
          println(CryptoService.keyToText(key))
          println("Без ключа открыть хранилище будет невозможно.")
          confirm("Подтвердите, что сохранили ключ", false).map(_ => KeyChoice(Some(key), false))
      }
    }
  }

  private def saveKey(path: Path, key: Array[Byte]): Unit = {
    KeyService.saveKey(config, path, key) match {
      case Right(savedPath) =>
        println(s"Ключ сохранён: $savedPath")
        println("Сохраните отдельную копию ключа в надёжном месте.")
      case Left(error) =>
        println(ErrorMessages.text(error))
        println("Сохраните ключ самостоятельно:")
        println(CryptoService.keyToText(key))
    }
  }

  private def addInitialSources(session: StorageSession): Either[AppError, Unit] = {
    var adding = true
    var result: Either[AppError, Unit] = Right(())
    while (adding && result.isRight) {
      result = InputHelper.readYesNo("Добавить источник резервного копирования?", false).flatMap {
        case true => addSource(session)
        case false =>
          adding = false
          Right(())
      }
    }
    result
  }

  private def openStorage(): Either[AppError, Unit] = {
    for {
      directory <- chooseDirectory(allowCreate = false)
      containers <- listContainers(directory)
      _ = containers.zipWithIndex.foreach { case (path, index) => println(s"${index + 1}. ${path.getFileName}") }
      answer <- InputHelper.read("Номер файла или его имя: ")
      path <- answer.toIntOption.filter(n => n >= 1 && n <= containers.size) match {
        case Some(number) => Right(containers(number - 1))
        case None =>
          if (answer.isEmpty) Left(AppError.InvalidPath(answer))
          else FileHelper.path(directory.resolve(answer).toString)
      }
      encrypted <- ContainerService.isEncrypted(path)
      key <- if (encrypted) readKey(path).map(Some(_)) else Right(None)
      session <- ContainerService.open(path, key)
    } yield {
      replaceCurrentStorage(session)
      showInfo(session)
      log.info(s"Открыто хранилище: $path")
      if (config.checkOnStorageOpen) scanStorage(session)
    }
  }

  private def readKey(path: Path): Either[AppError, Array[Byte]] = {
    val defaultPath = KeyService.defaultKeyPath(config, path)
    println("Хранилище зашифровано.")
    if (!Files.isRegularFile(defaultPath)) readKeyFromKeyboard()
    else {
      println(s"1. Использовать файл ключа: $defaultPath")
      println("2. Ввести ключ вручную")
      InputHelper.readInt("Выберите способ: ", 1, 2).flatMap {
        case 1 => KeyService.loadKey(defaultPath)
        case _ => readKeyFromKeyboard()
      }
    }
  }

  private def readKeyFromKeyboard(): Either[AppError, Array[Byte]] =
    InputHelper.read("Ключ доступа: ").flatMap(CryptoService.textToKey)

  private def showInfo(session: StorageSession): Unit = {
    println(s"Хранилище: ${session.data.name}")
    println(s"Файл: ${session.containerPath}")
    println(s"Шифрование: ${yesNo(session.encrypted)}")
    println(s"Источников: ${session.data.sources.size}")
    println(s"Версий: ${session.data.versions.size}")
    val last = session.data.versions.sortBy(_.id).lastOption.map(_.createdAt).getOrElse("отсутствует")
    println(s"Последняя копия: $last")
  }

  private def manageSources(session: StorageSession): Either[AppError, Unit] = {
    var managing = true
    var result: Either[AppError, Unit] = Right(())
    while (managing && result.isRight) {
      showSources(session)
      println("Команды: add, edit, remove, toggle, back")
      val action = InputHelper.read("sources> ").flatMap(SourceCommand.parse).flatMap {
        case SourceCommand.Add => addSource(session)
        case SourceCommand.Edit => editSource(session)
        case SourceCommand.Remove => removeSource(session)
        case SourceCommand.Toggle => toggleSource(session)
        case SourceCommand.Back =>
          managing = false
          Right(())
        case SourceCommand.Empty => Right(())
      }
      action match {
        case Left(AppError.InputClosed) => result = action
        case Left(error) => println(ErrorMessages.text(error))
        case Right(_) => ()
      }
    }
    result
  }

  private def showSources(session: StorageSession): Unit = {
    if (session.data.sources.isEmpty) println("Источники не настроены.")
    session.data.sources.foreach { source =>
      val state = SourceService.state(source) match {
        case SourceState.Enabled => "доступен"
        case SourceState.Disabled => "отключён"
        case SourceState.Unavailable => "недоступен"
      }
      println(s"${source.id}. ${source.path} ($state)")
      println(s"   Вложенные каталоги: ${yesNo(source.includeSubdirectories)}")
      println(s"   Исключения: ${source.exclusions.mkString(", ")}")
      println(s"   Последняя копия: ${source.lastSuccessfulBackup.getOrElse("отсутствует")}")
    }
  }

  private def addSource(session: StorageSession): Either[AppError, Unit] = {
    for {
      text <- InputHelper.read("Путь к файлу или каталогу: ")
      path <- FileHelper.path(text)
      include <- if (Files.isDirectory(path)) InputHelper.readYesNo("Включать вложенные каталоги?", true)
                 else Right(false)
      rules <- InputHelper.readWithDefault("Исключения через запятую (- — без исключений)", config.defaultExclusions.mkString(","))
      source <- SourceService.addSource(session, text, include, parseExclusions(rules))
    } yield {
      println(s"Источник добавлен под номером ${source.id}.")
      log.info(s"Добавлен источник: ${source.path}")
    }
  }

  private def editSource(session: StorageSession): Either[AppError, Unit] = {
    for {
      id <- InputHelper.readInt("Номер источника: ", 1, Int.MaxValue)
      source <- SourceService.findSource(session, id)
      path <- InputHelper.readWithDefault("Путь", source.path)
      include <- InputHelper.readYesNo("Включать вложенные каталоги?", source.includeSubdirectories)
      rules <- InputHelper.readWithDefault("Исключения (- — без исключений)", source.exclusions.mkString(","))
      _ <- SourceService.editSource(session, id, path, include, parseExclusions(rules))
    } yield {
      println("Источник изменён.")
      log.info(s"Изменён источник $id")
    }
  }

  private def removeSource(session: StorageSession): Either[AppError, Unit] = {
    for {
      id <- InputHelper.readInt("Номер источника: ", 1, Int.MaxValue)
      _ <- SourceService.findSource(session, id)
      _ <- confirm("Удалить источник из списка? Сохранённые версии останутся.", false)
      _ <- SourceService.removeSource(session, id)
    } yield {
      println("Источник удалён из списка.")
      log.info(s"Удалён источник $id")
    }
  }

  private def toggleSource(session: StorageSession): Either[AppError, Unit] = {
    for {
      id <- InputHelper.readInt("Номер источника: ", 1, Int.MaxValue)
      enabled <- SourceService.toggleSource(session, id)
    } yield {
      println(if (enabled) "Источник включён." else "Источник отключён.")
      log.info(s"Изменено состояние источника $id")
    }
  }

  private def scanStorage(session: StorageSession): Unit = {
    val result = ScanService.scan(session)
    printScanResult(result)
    log.info(s"Проверка завершена: изменений ${result.changes.size}, проблем ${result.problems.size}")
  }

  private def printScanResult(result: ScanResult): Unit = {
    println(s"Новые файлы: ${result.addedCount}")
    println(s"Изменённые файлы: ${result.modifiedCount}")
    println(s"Удалённые файлы: ${result.deletedCount}")
    println(s"Переименованные или перемещённые: ${result.movedCount}")
    println(s"Объём файлов с изменениями: ${InputHelper.formatBytes(result.dataSize)}")
    result.problems.foreach(error => println(ErrorMessages.text(error)))
    result.changes.sortBy(_.relativePath).foreach { change =>
      val description = change.changeType match {
        case ChangeType.ADDED => "новый"
        case ChangeType.MODIFIED => "изменён"
        case ChangeType.DELETED => "удалён"
        case ChangeType.MOVED => s"перемещён из ${change.previousRelativePath.getOrElse("неизвестного пути")}"
      }
      println(s"- [$description] ${change.relativePath}")
    }
  }

  private def createBackup(session: StorageSession): Either[AppError, Unit] = {
    val scan = ScanService.scan(session)
    printScanResult(scan)
    if (scan.changes.isEmpty) return Left(AppError.NoChanges)
    for {
      _ <- if (config.confirmBeforeBackup) confirm("Добавить эти изменения в хранилище?", false) else Right(())
      comment <- InputHelper.read("Комментарий к версии (можно оставить пустым): ")
      _ = log.info("Начато резервное копирование")
      result <- BackupService.createBackup(session, scan, Option(comment).filter(_.nonEmpty), config)
    } yield {
      println(s"Создана версия ${result.versionId}.")
      println(s"Добавлено: ${result.added}, обновлено: ${result.modified}, удалено: ${result.deleted}, перемещено: ${result.moved}")
      println(s"Пропущено: ${result.skipped}, записано: ${InputHelper.formatBytes(result.writtenBytes)}")
      result.problems.foreach(error => println(ErrorMessages.text(error)))
      log.info(s"Создана версия ${result.versionId}, проблем ${result.problems.size}")
    }
  }

  private def showVersions(session: StorageSession): Unit = {
    if (session.data.versions.isEmpty) println("Сохранённых версий нет.")
    session.data.versions.sortBy(_.id).reverse.foreach { version =>
      val status = version.result match {
        case BackupStatus.Complete => "успешно"
        case BackupStatus.Partial => "частично"
      }
      val comment = version.comment.map(" — " + _).getOrElse("")
      println(s"${version.id}. ${version.createdAt} +${version.added} ~${version.modified} -${version.deleted} >${version.moved} $status$comment")
    }
  }

  private def restoreFiles(session: StorageSession): Either[AppError, Unit] = {
    showVersions(session)
    for {
      latest <- session.data.versions.map(_.id).maxOption.toRight(AppError.NoVersions)
      text <- InputHelper.readWithDefault("Номер версии", latest.toString)
      version <- text.toIntOption.toRight(AppError.InvalidNumber(text))
      destinationText <- InputHelper.read("Каталог назначения: ")
      destination <- FileHelper.path(destinationText)
      plan <- RestoreService.plan(session, version, destination)
      allowed <- collectOverwriteAnswers(plan)
      _ = log.info(s"Начато восстановление версии $version")
      result <- RestoreService.restore(session, plan, allowed)
    } yield {
      println(s"Восстановлено: ${result.restored}, пропущено: ${result.skipped}, ошибок: ${result.failed}")
      result.problems.foreach(error => println(ErrorMessages.text(error)))
      log.info(s"Восстановление завершено: файлов ${result.restored}, ошибок ${result.failed}")
    }
  }

  private def collectOverwriteAnswers(plan: RestorePlan): Either[AppError, Set[Path]] = {
    plan.items.foldLeft[Either[AppError, Set[Path]]](Right(Set.empty)) { (result, item) =>
      result.flatMap { allowed =>
        if (config.confirmBeforeRestoreOverwrite && Files.exists(item.target)) {
          InputHelper.readYesNo(s"Файл ${item.target} уже существует. Перезаписать?", false)
            .map(answer => if (answer) allowed + item.target else allowed)
        } else if (!config.confirmBeforeRestoreOverwrite && config.overwriteExistingWithoutConfirmation)
          Right(allowed + item.target)
        else Right(allowed)
      }
    }
  }

  private def verifyStorage(session: StorageSession): Unit = {
    val result = VerifyService.verify(session)
    println(s"Проверено блоков данных: ${result.checked}")
    if (result.problems.isEmpty) println("Хранилище не повреждено.")
    else result.problems.foreach(error => println(ErrorMessages.text(error)))
    log.info(s"Проверка целостности: блоков ${result.checked}, проблем ${result.problems.size}")
  }

  private def reloadConfig(): Either[AppError, Unit] = {
    ConfigService.load(configPath).map { loaded =>
      config = loaded
      log.updatePath(loaded.logFile)
      log.info("Конфигурация перезагружена")
      println("Конфигурация перезагружена.")
    }
  }

  private def chooseDirectory(allowCreate: Boolean): Either[AppError, Path] = {
    val roots = FileSystems.getDefault.getRootDirectories.asScala.toList
    println("Доступные устройства:")
    roots.zipWithIndex.foreach { case (root, index) => println(s"${index + 1}. $root") }
    for {
      answer <- InputHelper.read("Номер устройства или полный путь к каталогу: ")
      directory <- answer.toIntOption.filter(n => n >= 1 && n <= roots.size) match {
        case Some(number) =>
          InputHelper.read("Каталог на устройстве (Enter — корень): ")
            .flatMap(folder => FileHelper.path(roots(number - 1).resolve(folder).toString))
        case None => FileHelper.path(answer)
      }
      _ <- if (allowCreate && !Files.exists(directory)) {
        confirm("Каталог не существует. Создать?", true).flatMap { _ =>
          FileHelper.attempt("создание каталога", directory.toString)(Files.createDirectories(directory)).map(_ => ())
        }
      } else Right(())
      _ <- if (!Files.isDirectory(directory) || (allowCreate && !Files.isWritable(directory)))
        Left(AppError.DestinationUnavailable(directory.toString)) else Right(())
    } yield directory
  }

  private def listContainers(directory: Path): Either[AppError, List[Path]] = {
    FileHelper.attempt("список контейнеров", directory.toString) {
      val files = Files.list(directory)
      try files.iterator().asScala.filter(Files.isRegularFile(_))
        .filter(_.getFileName.toString.toLowerCase.endsWith(".store")).toList.sortBy(_.getFileName.toString)
      finally files.close()
    }
  }

  private def validateFileName(name: String): Either[AppError, Unit] = {
    if (name.isEmpty || name == "." || name == ".." || name.exists(c => c == '/' || c == '\\'))
      Left(AppError.InvalidPath(name))
    else Right(())
  }

  private def replaceCurrentStorage(session: StorageSession): Unit = {
    currentStorage.foreach(ContainerService.close)
    currentStorage = Some(session)
  }

  private def withStorage(action: StorageSession => Either[AppError, Unit]): Either[AppError, Unit] =
    currentStorage.toRight(AppError.StorageNotOpen).flatMap(action)

  private def confirm(prompt: String, default: Boolean): Either[AppError, Unit] =
    InputHelper.readYesNo(prompt, default).flatMap(answer => if (answer) Right(()) else Left(AppError.Cancelled))

  private def parseExclusions(text: String): List[String] =
    if (text == "-") Nil else text.split(",").map(_.trim).filter(_.nonEmpty).toList

  private def yesNo(value: Boolean): String = if (value) "да" else "нет"
}
