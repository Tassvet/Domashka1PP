// здесь находится консольный интерфейс: читаем команды и вызываем нужные сервисы
// эта часть задаёт вопросы пользователю; само копирование выполняется в BackupService

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.file.{FileSystems, Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

class ConsoleApp(
    // путь к конфигурационному файлу
    configPath: Path,
    // настройки, полученные при запуске
    initialConfig: AppConfig,
    // журнал для записи событий
    log: LogService
) {
  private var config = initialConfig
  // текущая сессия; None означает, что хранилище ещё не открыто
  private var currentStorage: Option[StorageSession] = None
  // последний результат проверки; None означает, что сохранённого результата нет
  private var lastScan: Option[ScanResult] = None
  // флаг основного цикла: false завершает работу
  private var running = true

  // основной цикл: ждём команду и вызываем нужный метод этого класса
  // работаем до exit; в конце закрываем текущую сессию и записываем завершение в журнал
  def run(): Unit = {
    log.info("Приложение запущено")
    // выводим смску или результат юзеру
    println("Переносное резервное хранилище")
    // выводим смску или результат юзеру
    println("Введите help, чтобы увидеть список команд.")

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // прога ждёт команды, пока флаг running равен true
      while (running) {
        // читаем команду и приводим её к нижнему регистру
        val command = InputHelper.read("\n> ").toLowerCase

        // операции могут закончиться ошибкой; ниже catch обработает исключение
        try {
          // сравниваем введённую команду с разрешёнными вариантами
          command match {
            // для команды create вызываем createStorage
            case "create"   => createStorage()
            // для команды open вызываем openStorage
            case "open"     => openStorage()
            // для команды info вызываем showInfo
            case "info"     => withStorage(showInfo)
            // для команды sources вызываем manageSources
            case "sources"  => withStorage(manageSources)
            // для команды scan вызываем session
            case "scan"     => withStorage(session => scanStorage(session, showFiles = true))
            // для команды backup вызываем createBackup
            case "backup"   => withStorage(createBackup)
            // для команды versions вызываем showVersions
            case "versions" => withStorage(showVersions)
            // для команды restore вызываем restoreFiles
            case "restore"  => withStorage(restoreFiles)
            // для команды verify вызываем verifyStorage
            case "verify"   => withStorage(verifyStorage)
            // для команды reload вызываем reloadConfig
            case "reload"   => reloadConfig()
            // для команды help вызываем showHelp
            case "help"     => showHelp()
            // exit меняет флаг: основной цикл заканчивается
            case "exit"     => running = false
            case ""         => ()
            case _ =>
              // выводим смску или результат юзеру
              println("Неизвестная команда. Введите help для просмотра команд.")
              log.warning("Введена неизвестная команда")
          }
        } catch {
          // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
          case NonFatal(error) =>
            // выводим смску или результат юзеру
            println(s"Операция не выполнена: ${error.getMessage}")
            log.error(s"Внутренняя ошибка: ${error.getClass.getSimpleName}: ${error.getMessage}")
        }
      }
    } catch {
      // при закрытом вводе заканчиваем работу приложения
      case _: InputEndedException => running = false
    // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
    } finally {
      currentStorage.foreach(ContainerService.close)
      log.info("Приложение завершено")
      // выводим смску или результат юзеру
      println("Работа завершена.")
    }
  }

  // выводим список доступных команд и их назначение
  private def showHelp(): Unit = {
    // выводим смску или результат юзеру
    println(
      """
        |Доступные команды:
        |create   - создать новое хранилище
        |open     - открыть существующее хранилище
        |info     - показать сведения о хранилище
        |sources  - настроить резервируемые файлы и каталоги
        |scan     - проверить наличие изменений
        |backup   - добавить изменения в хранилище
        |versions - показать сохранённые версии
        |restore  - восстановить файлы
        |verify   - проверить целостность хранилища
        |reload   - перезагрузить конфигурацию
        |exit     - завершить работу
        |""".stripMargin
    )
  }

  // спрашиваем каталог, имя контейнера, название и способ работы с ключом
  // ContainerService.create создаёт хранилище; после этого предлагаем добавить источники
  private def createStorage(): Unit = {
    chooseDirectory(allowCreate = true) match {
      // значения нет: например, ничего не найдено или ответ не получен
      case None => ()
      // значение есть; извлекаем его из Some и используем в этой ветке
      case Some(directory) =>
        // имя файла без каталога
        val fileName = InputHelper.readWithDefault(
          "Имя файла хранилища",
          config.defaultContainerName
        )

        if (!isSimpleFileName(fileName)) {
          // выводим смску или результат юзеру
          println("Имя файла не должно содержать путь к каталогу.")
          return
        }

        // путь к переносимому контейнеру
        val containerPath = directory.resolve(fileName).toAbsolutePath.normalize()
        // проверяем, существует ли указанный путь
        if (Files.exists(containerPath) &&
            !InputHelper.readYesNo("Файл уже существует. Перезаписать его?", false)) {
          // выводим смску или результат юзеру
          println("Создание отменено.")
          return
        }

        // название хранилища
        val storageName = InputHelper.readWithDefault("Название хранилища", "Моя резервная копия")
        // признак включённого шифрования
        val encrypted = InputHelper.readYesNo("Использовать шифрование?", false)
        // создаём или получаем ключ шифрования
        val key = if (encrypted) Some(CryptoService.generateKey()) else None
        // запоминаем, выбрано ли сохранение ключа
        var saveKeyToFile = false

        if (encrypted) {
          // выводим смску или результат юзеру
          println("\nСпособ хранения ключа:")
          // выводим смску или результат юзеру
          println("1. Сохранить ключ в каталоге из конфигурации")
          // выводим смску или результат юзеру
          println("2. Показать ключ и не сохранять его")
          // номер выбранного пункта
          val choice = InputHelper.readInt("Выберите способ: ", 1, 2)
          saveKeyToFile = choice == 1

          if (!saveKeyToFile) {
            // выводим смску или результат юзеру
            println("\nКлюч доступа к хранилищу:\n")
            // выводим смску или результат юзеру
            println(CryptoService.keyToText(key.get))
            // выводим смску или результат юзеру
            println("\nПрограмма не будет сохранять этот ключ.")
            // выводим смску или результат юзеру
            println("Без него открыть хранилище будет невозможно.")

            if (!InputHelper.readYesNo("Подтвердите, что ключ сохранён", false)) {
              // выводим смску или результат юзеру
              println("Создание отменено. Сначала сохраните ключ в надёжном месте.")
              return
            }
          }
        }

        ContainerService.create(
          containerPath,
          storageName,
          encrypted,
          key,
          config.hashAlgorithm
        ) match {
          // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
          case Left(error) =>
            // выводим смску или результат юзеру
            println(error)
            log.error(s"Не удалось создать хранилище: $containerPath")

          // эта ветка выполняется если всё кул
          case Right(session) =>
            replaceCurrentStorage(session)
            log.info(s"Создано хранилище: $containerPath")
            // выводим смску или результат юзеру
            println(s"Хранилище создано: $containerPath")

            if (saveKeyToFile) {
              KeyService.saveKey(config, containerPath, key.get) match {
                // эта ветка выполняется если всё кул
                case Right(keyPath) =>
                  // выводим смску или результат юзеру
                  println(s"Ключ сохранён в файле: $keyPath")
                  // выводим смску или результат юзеру
                  println("Не храните единственную копию ключа на устройстве с резервной копией.")
                // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
                case Left(error) =>
                  // выводим смску или результат юзеру
                  println(error)
                  // выводим смску или результат юзеру
                  println("Сохраните следующий ключ самостоятельно:")
                  // выводим смску или результат юзеру
                  println(CryptoService.keyToText(key.get))
              }
            }

            while (InputHelper.readYesNo("Добавить источник резервного копирования?", false)) {
              addSource(session)
            }
        }
    }
  }

  // выбираем существующий контейнер, при необходимости читаем ключ
  // ContainerService.open открывает сессию; затем показываем сведения и при настройке запускаем scan
  private def openStorage(): Unit = {
    chooseDirectory(allowCreate = false) match {
      // значения нет: например, ничего не найдено или ответ не получен
      case None => ()
      // значение есть; извлекаем его из Some и используем в этой ветке
      case Some(directory) =>
        // найденные файлы .store
        val containers = listContainers(directory)

        if (containers.nonEmpty) {
          // выводим смску или результат юзеру
          println("\nНайденные файлы хранилищ:")
          containers.zipWithIndex.foreach { case (path, index) =>
            // выводим смску или результат юзеру
            println(s"${index + 1}. ${path.getFileName}")
          }
        }

        // выбираем приглашение к вводу
        val prompt = if (containers.nonEmpty) {
          "Введите номер файла или его имя: "
        } else {
          "Введите имя файла хранилища: "
        }

        // ответ пользователя
        val answer = InputHelper.read(prompt)
        // путь к переносимому контейнеру
        val containerPath = answer.toIntOption match {
          // значение есть; извлекаем его из Some и используем в этой ветке
          case Some(number) if number >= 1 && number <= containers.size => containers(number - 1)
          case _ =>
            // превращаем введённую строку в путь
            val enteredPath = Paths.get(answer)
            if (enteredPath.isAbsolute) enteredPath else directory.resolve(enteredPath)
        }

        // делаем путь абсолютным и убираем лишние части
        val normalizedPath = containerPath.toAbsolutePath.normalize()
        ContainerService.isEncrypted(normalizedPath) match {
          // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
          case Left(error) => println(error)
          // эта ветка выполняется если всё кул
          case Right(encrypted) =>
            // создаём или получаем ключ шифрования
            val key = if (encrypted) readKey(normalizedPath) else Some(None)

            key match {
              // значения нет: например, ничего не найдено или ответ не получен
              case None => ()
              // значение есть; извлекаем его из Some и используем в этой ветке
              case Some(actualKey) =>
                ContainerService.open(normalizedPath, actualKey) match {
                  // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
                  case Left(error) =>
                    // выводим смску или результат юзеру
                    println(error)
                    log.warning(s"Неудачная попытка открыть хранилище: $normalizedPath")
                  // эта ветка выполняется если всё кул
                  case Right(session) =>
                    replaceCurrentStorage(session)
                    log.info(s"Открыто хранилище: $normalizedPath")
                    showInfo(session)

                    // автоматическая проверка выполняется, только если она разрешена в настройках
                    if (config.checkOnStorageOpen) {
                      scanStorage(session, showFiles = false)
                    }
                }
            }
        }
    }
  }

  // предлагаем найденный файл ключа или ручной ввод
  // внешний Option показывает, удалось ли получить ответ; внутренний Option содержит ключ
  private def readKey(containerPath: Path): Option[Option[Array[Byte]]] = {
    // ожидаемый путь отдельного файла ключа
    val defaultPath = KeyService.defaultKeyPath(config, containerPath)
    // выводим смску или результат юзеру
    println("Хранилище зашифровано.")

    if (Files.isRegularFile(defaultPath)) {
      // выводим смску или результат юзеру
      println(s"1. Использовать найденный файл ключа: $defaultPath")
      // выводим смску или результат юзеру
      println("2. Ввести ключ вручную")
      // номер выбранного пункта
      val choice = InputHelper.readInt("Выберите способ: ", 1, 2)

      if (choice == 1) {
        KeyService.loadKey(defaultPath) match {
          // эта ветка выполняется если всё кул
          case Right(key)  => Some(Some(key))
          // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
          case Left(error) =>
            // выводим смску или результат юзеру
            println(error)
            None
        }
      } else {
        readKeyFromKeyboard()
      }
    } else {
      // выводим смску или результат юзеру
      println("Файл ключа рядом с программой не найден.")
      readKeyFromKeyboard()
    }
  }

  // читаем ключ и проверяем его формат через CryptoService.textToKey
  // сам введённый ключ в журнал не записываем
  private def readKeyFromKeyboard(): Option[Option[Array[Byte]]] = {
    // получаем текст для дальнейшей обработки
    val text = InputHelper.read("Введите ключ доступа: ")
    CryptoService.textToKey(text) match {
      // эта ветка выполняется если всё кул
      case Right(key)  => Some(Some(key))
      // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
      case Left(error) =>
        // выводим смску или результат юзеру
        println(error)
        None
    }
  }

  // показываем название, путь, шифрование, число источников и сведения о последней версии
  private def showInfo(session: StorageSession): Unit = {
    // последняя резервная версия; если истории нет, получаем None
    val lastVersion = session.data.versions.sortBy(_.id).lastOption
    // выводим смску или результат юзеру
    println(s"\nХранилище: ${session.data.name}")
    // выводим смску или результат юзеру
    println(s"Файл: ${session.containerPath}")
    // выводим смску или результат юзеру
    println(s"Шифрование: ${if (session.encrypted) "включено" else "выключено"}")
    // выводим смску или результат юзеру
    println(s"Источников: ${session.data.sources.size}")
    // выводим смску или результат юзеру
    println(s"Последняя резервная копия: ${lastVersion.map(_.createdAt).getOrElse("отсутствует")}")
    // выводим смску или результат юзеру
    println(s"Сохранённых версий: ${session.data.versions.size}")
  }

  // отдельное меню для add, edit, remove и toggle
  // команда back возвращает в основное меню
  private def manageSources(session: StorageSession): Unit = {
    // пока true, остаёмся в меню источников
    var managing = true

    // остаёмся в меню источников до команды back
    while (managing) {
      showSources(session)
      // выводим смску или результат юзеру
      println("\nКоманды источников: add, edit, remove, toggle, back")

      InputHelper.read("sources> ").toLowerCase match {
        // для команды add вызываем addSource
        case "add"    => addSource(session)
        // для команды edit вызываем editSource
        case "edit"   => editSource(session)
        // для команды remove вызываем removeSource
        case "remove" => removeSource(session)
        // для команды toggle вызываем toggleSource
        case "toggle" => toggleSource(session)
        // back завершает только меню источников
        case "back"   => managing = false
        case _         => println("Неизвестная команда управления источниками.")
      }
    }
  }

  // выводим каждый источник вместе с настройками и текущей доступностью
  private def showSources(session: StorageSession): Unit = {
    // выводим смску или результат юзеру
    println("\nИсточники резервного копирования:")

    // без источников проверять и копировать нечего
    if (session.data.sources.isEmpty) {
      // выводим смску или результат юзеру
      println("Источники не настроены.")
    } else {
      session.data.sources.foreach { source =>
        // выводим смску или результат юзеру
        println(s"\n${source.id}. ${source.path}")
        // выводим смску или результат юзеру
        println(s"   Вложенные каталоги: ${yesNo(source.includeSubdirectories)}")
        // выводим смску или результат юзеру
        println(s"   Исключения: ${if (source.exclusions.isEmpty) "нет" else source.exclusions.mkString(", ")}")
        // выводим смску или результат юзеру
        println(s"   Состояние: ${SourceService.state(source)}")
        // выводим смску или результат юзеру
        println(s"   Последняя копия: ${source.lastSuccessfulBackup.getOrElse("отсутствует")}")
      }
    }
  }

  // читаем путь и настройки источника, затем вызываем SourceService.addSource
  // после успешного добавления сбрасываем прежний результат проверки
  private def addSource(session: StorageSession): Unit = {
    // путь, введённый пользователем
    val pathText = InputHelper.read("Путь к файлу или каталогу: ")
    if (pathText.isEmpty) {
      // выводим смску или результат юзеру
      println("Путь не указан.")
      return
    }

    val path = Paths.get(pathText)
    // нужно ли включать вложенные папки
    val includeSubdirectories = if (Files.isDirectory(path)) {
      InputHelper.readYesNo("Включать вложенные каталоги?", true)
    } else {
      false
    }

    // исключения из конфигурации в виде строки
    val defaultRules = config.defaultExclusions.mkString(",")
    // получаем список исключений
    val exclusions = parseExclusions(
      InputHelper.readWithDefault("Исключения через запятую", defaultRules)
    )

    SourceService.addSource(session, pathText, includeSubdirectories, exclusions) match {
      // эта ветка выполняется если всё кул
      case Right(source) =>
        // выводим смску или результат юзеру
        println(s"Источник добавлен под номером ${source.id}.")
        log.info(s"Добавлен источник: ${source.path}")
        // старый результат проверки больше не используем
        lastScan = None
      // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
      case Left(error) => println(error)
    }
  }

  // получаем номер и новые настройки; SourceService.editSource сохраняет изменения
  private def editSource(session: StorageSession): Unit = {
    // без источников проверять и копировать нечего
    if (session.data.sources.isEmpty) {
      // выводим смску или результат юзеру
      println("Источники не настроены.")
      return
    }

    // номер выбранного источника
    val id = InputHelper.readInt("Номер источника: ", 1, Int.MaxValue)
    session.data.sources.find(_.id == id) match {
      // значения нет: например, ничего не найдено или ответ не получен
      case None => println("Источник с таким номером не найден.")
      // значение есть; извлекаем его из Some и используем в этой ветке
      case Some(source) =>
        val path = InputHelper.readWithDefault("Путь", source.path)
        // новая настройка вложенных каталогов
        val include = InputHelper.readYesNo(
          "Включать вложенные каталоги?",
          source.includeSubdirectories
        )
        // получаем список исключений
        val exclusions = parseExclusions(
          InputHelper.readWithDefault("Исключения через запятую", source.exclusions.mkString(","))
        )

        SourceService.editSource(session, id, path, include, exclusions) match {
          // эта ветка выполняется если всё кул
          case Right(_) =>
            // выводим смску или результат юзеру
            println("Источник изменён.")
            log.info(s"Изменён источник номер $id")
            // старый результат проверки больше не используем
            lastScan = None
          // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
          case Left(error) => println(error)
        }
    }
  }

  // просим подтвердить удаление источника из списка и вызываем SourceService.removeSource
  private def removeSource(session: StorageSession): Unit = {
    // без источников проверять и копировать нечего
    if (session.data.sources.isEmpty) {
      // выводим смску или результат юзеру
      println("Источники не настроены.")
      return
    }

    // номер выбранного источника
    val id = InputHelper.readInt("Номер источника: ", 1, Int.MaxValue)
    if (InputHelper.readYesNo("Удалить источник из списка? Сохранённые версии останутся.", false)) {
      SourceService.removeSource(session, id) match {
        // эта ветка выполняется если всё кул
        case Right(_) =>
          // выводим смску или результат юзеру
          println("Источник удалён из списка.")
          log.info(s"Удалён источник номер $id")
          // старый результат проверки больше не используем
          lastScan = None
        // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
        case Left(error) => println(error)
      }
    }
  }

  // выбираем источник и вызываем SourceService.toggleSource для включения или отключения
  private def toggleSource(session: StorageSession): Unit = {
    // без источников проверять и копировать нечего
    if (session.data.sources.isEmpty) {
      // выводим смску или результат юзеру
      println("Источники не настроены.")
      return
    }

    // номер выбранного источника
    val id = InputHelper.readInt("Номер источника: ", 1, Int.MaxValue)
    SourceService.toggleSource(session, id) match {
      // эта ветка выполняется если всё кул
      case Right(enabled) =>
        // выводим смску или результат юзеру
        println(if (enabled) "Источник включён." else "Источник временно отключён.")
        log.info(s"Изменено состояние источника номер $id")
        // старый результат проверки больше не используем
        lastScan = None
      // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
      case Left(error) => println(error)
    }
  }

  // вызываем ScanService.scan и показываем результат
  // хранилище от этой команды не меняется
  private def scanStorage(session: StorageSession, showFiles: Boolean): Unit = {
    // без источников проверять и копировать нечего
    if (session.data.sources.isEmpty) {
      // выводим смску или результат юзеру
      println("Сначала добавьте хотя бы один источник.")
      return
    }

    // результат обработки
    val result = ScanService.scan(session, config, log)
    lastScan = Some(result)
    printScanResult(result, showFiles)
  }

  // показываем количества изменений, объём, ошибки чтения и при необходимости список файлов
  private def printScanResult(result: ScanResult, showFiles: Boolean): Unit = {
    // выводим смску или результат юзеру
    println("\nОбнаружены изменения:")
    // выводим смску или результат юзеру
    println(s"Новые файлы: ${result.addedCount}")
    // выводим смску или результат юзеру
    println(s"Изменённые файлы: ${result.modifiedCount}")
    // выводим смску или результат юзеру
    println(s"Удалённые файлы: ${result.deletedCount}")
    // выводим смску или результат юзеру
    println(s"Переименованные или перемещённые файлы: ${result.movedCount}")
    // выводим смску или результат юзеру
    println(s"Недоступные файлы: ${result.unreadableFiles.size}")
    // выводим смску или результат юзеру
    println(s"Недоступные источники: ${result.unavailableSources.size}")
    // выводим смску или результат юзеру
    println(s"Общий объём новых данных: ${InputHelper.formatBytes(result.dataSize)}")

    if (result.unavailableSources.nonEmpty) {
      // выводим смску или результат юзеру
      println("\nНе удалось проверить источники:")
      result.unavailableSources.foreach(path => println(s"- $path"))
    }

    if (result.unreadableFiles.nonEmpty) {
      // выводим смску или результат юзеру
      println("\nНе удалось прочитать файлы:")
      result.unreadableFiles.foreach(path => println(s"- $path"))
    }

    if (showFiles && result.changes.nonEmpty) {
      // выводим смску или результат юзеру
      println("\nСписок изменений:")
      result.changes.sortBy(change => (change.changeType, change.relativePath)).foreach { change =>
        // описание файловой системы диска
        val description = change.changeType match {
          case "ADDED"    => "новый"
          case "MODIFIED" => "изменён"
          case "DELETED"  => "удалён"
          // для команды MOVED вызываем s
          case "MOVED"    => s"перемещён из ${change.previousRelativePath.getOrElse("?")}"
          case other      => other
        }
        // выводим смску или результат юзеру
        println(s"- [$description] ${change.relativePath}")
      }
    }
  }

  // перед сохранением снова вызываем ScanService, выводим изменения и спрашиваем подтверждение
  // после этого BackupService создаёт версию, а мы показываем результат
  private def createBackup(session: StorageSession): Unit = {
    // без источников проверять и копировать нечего
    if (session.data.sources.isEmpty) {
      // выводим смску или результат юзеру
      println("Сначала добавьте хотя бы один источник.")
      return
    }

    // результат обработки
    val result = ScanService.scan(session, config, log)
    lastScan = Some(result)
    printScanResult(result, showFiles = true)

    if (result.changes.isEmpty) {
      // выводим смску или результат юзеру
      println("Изменений для сохранения нет.")
      return
    }

    if (config.confirmBeforeBackup &&
        !InputHelper.readYesNo("Добавить эти изменения в хранилище?", false)) {
      // выводим смску или результат юзеру
      println("Резервное копирование отменено.")
      return
    }

    val comment = InputHelper.read("Комментарий к версии (можно оставить пустым): ")
    BackupService.createBackup(session, result, comment, config, log) match {
      // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
      case Left(error) => println(error)
      // эта ветка выполняется если всё кул
      case Right(backup) =>
        // старый результат проверки больше не используем
        lastScan = None
        // выводим смску или результат юзеру
        println("\nРезервное копирование завершено.")
        // выводим смску или результат юзеру
        println(s"Добавлено файлов: ${backup.added}")
        // выводим смску или результат юзеру
        println(s"Обновлено файлов: ${backup.modified}")
        // выводим смску или результат юзеру
        println(s"Отмечено удалённых файлов: ${backup.deleted}")
        // выводим смску или результат юзеру
        println(s"Перемещено файлов: ${backup.moved}")
        // выводим смску или результат юзеру
        println(s"Пропущено файлов: ${backup.skipped}")
        // выводим смску или результат юзеру
        println(s"Записано данных: ${InputHelper.formatBytes(backup.writtenBytes)}")
        // выводим смску или результат юзеру
        println(s"Версия хранилища: ${backup.versionId}")
    }
  }

  // показываем версии от новой к старой: дату, количества изменений и комментарий
  private def showVersions(session: StorageSession): Unit = {
    // выводим смску или результат юзеру
    println("\nВерсии хранилища:")

    // если версий ещё нет, показывать или восстанавливать нечего
    if (session.data.versions.isEmpty) {
      // выводим смску или результат юзеру
      println("Сохранённых версий нет.")
    } else {
      session.data.versions.sortBy(_.id).reverse.foreach { version =>
        val comment = if (version.comment.isEmpty) "" else s"  ${version.comment}"
        // выводим смску или результат юзеру
        println(
          f"${version.id}%4d  ${version.createdAt}  +${version.added}%d  " +
            f"~${version.modified}%d  -${version.deleted}%d  >${version.moved}%d  " +
            s"${version.result}$comment"
        )
      }
    }
  }

  // спрашиваем версию и папку; RestoreService выполняет восстановление
  // функция подтверждения передаётся отдельно для уже существующих файлов
  private def restoreFiles(session: StorageSession): Unit = {
    // если версий ещё нет, показывать или восстанавливать нечего
    if (session.data.versions.isEmpty) {
      // выводим смску или результат юзеру
      println("Сохранённых версий нет.")
      return
    }

    showVersions(session)
    // наибольший номер сохранённой версии
    val latestId = session.data.versions.map(_.id).max
    // введённый номер либо номер последней версии
    val versionText = InputHelper.readWithDefault("Номер версии", latestId.toString)

    versionText.toIntOption match {
      // значения нет: например, ничего не найдено или ответ не получен
      case None => println("Номер версии введён неправильно.")
      // значение есть; извлекаем его из Some и используем в этой ветке
      case Some(versionId) =>
        // строка с каталогом восстановления
        val destinationText = InputHelper.read("Каталог назначения: ")
        if (destinationText.isEmpty) {
          // выводим смску или результат юзеру
          println("Каталог назначения не указан.")
          return
        }

        // абсолютный путь к папке восстановления
        val destination = Paths.get(destinationText).toAbsolutePath.normalize()
        RestoreService.restore(
          session,
          versionId,
          destination,
          config,
          path => InputHelper.readYesNo(s"Файл $path уже существует. Перезаписать?", false),
          log
        ) match {
          // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
          case Left(error) => println(error)
          // эта ветка выполняется если всё кул
          case Right(result) =>
            // выводим смску или результат юзеру
            println("\nВосстановление завершено.")
            // выводим смску или результат юзеру
            println(s"Восстановлено файлов: ${result.restored}")
            // выводим смску или результат юзеру
            println(s"Пропущено файлов: ${result.skipped}")
            // выводим смску или результат юзеру
            println(s"Ошибок: ${result.failed}")
        }
    }
  }

  // вызываем VerifyService и показываем список обнаруженных проблем
  private def verifyStorage(session: StorageSession): Unit = {
    // результат обработки
    val result = VerifyService.verify(session, config, log)
    // выводим смску или результат юзеру
    println(s"Проверено файлов данных: ${result.checked}")

    if (result.problems.isEmpty) {
      // выводим смску или результат юзеру
      println("Хранилище не повреждено.")
    } else {
      // выводим смску или результат юзеру
      println("Обнаружены проблемы:")
      result.problems.foreach(problem => println(s"- $problem"))
    }
  }

  // повторно читаем настройки; при ошибке продолжаем использовать прежние
  private def reloadConfig(): Unit = {
    ConfigService.load(configPath) match {
      // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
      case Left(errors) =>
        // выводим смску или результат юзеру
        println("Не удалось перезагрузить конфигурацию:")
        // для каждой ошибки выводим строку; error => задаёт маленькую функцию
        errors.foreach(error => println(s"- $error"))
        log.warning("Конфигурацию не удалось перезагрузить")
      // эта ветка выполняется если всё кул
      case Right(newConfig) =>
        config = newConfig
        log.updatePath(newConfig.logFile)
        log.info("Конфигурация перезагружена")
        // выводим смску или результат юзеру
        println("Конфигурация перезагружена.")
    }
  }

  // показываем корни дисков и принимаем номер устройства либо путь к папке
  // для создания хранилища при подтверждении можно создать отсутствующую папку
  private def chooseDirectory(allowCreate: Boolean): Option[Path] = {
    // корневые каталоги доступных дисков
    val roots = FileSystems.getDefault.getRootDirectories.asScala.toList
    // выводим смску или результат юзеру
    println("\nДоступные устройства:")

    roots.zipWithIndex.foreach { case (root, index) =>
      // описание файловой системы диска
      val description = try {
        val store = Files.getFileStore(root)
        s"${store.name()} (${store.`type`()})"
      } catch {
        // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
        case NonFatal(_) => "описание недоступно"
      }
      // выводим смску или результат юзеру
      println(s"${index + 1}. $root - $description")
    }

    log.info(s"Обнаружено доступных устройств: ${roots.size}")
    // ответ пользователя
    val answer = InputHelper.read("Номер устройства или полный путь к каталогу: ")

    // выбранный пользователем путь
    val selected = answer.toIntOption match {
      // значение есть; извлекаем его из Some и используем в этой ветке
      case Some(number) if number >= 1 && number <= roots.size =>
        // корень выбранного источника или диск
        val root = roots(number - 1)
        // дополнительная папка внутри диска
        val folder = InputHelper.read("Каталог на устройстве (Enter — корень): ")
        if (folder.isEmpty) root else root.resolve(folder)
      case _ if answer.nonEmpty => Paths.get(answer)
      case _ =>
        // выводим смску или результат юзеру
        println("Каталог не выбран.")
        // возвращаем отсутствие результата; выбор не выполнен
        return None
    }

    // выбранный каталог
    val directory = selected.toAbsolutePath.normalize()

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      if (allowCreate && !Files.exists(directory)) {
        if (InputHelper.readYesNo("Каталог не существует. Создать его?", true)) {
          // создаём каталог и отсутствующие родительские папки
          Files.createDirectories(directory)
        } else {
          // возвращаем отсутствие результата; выбор не выполнен
          return None
        }
      }

      if (!Files.isDirectory(directory)) {
        // выводим смску или результат юзеру
        println("Указанный путь не является каталогом.")
        None
      } else if (allowCreate && !Files.isWritable(directory)) {
        // выводим смску или результат юзеру
        println("Каталог недоступен для записи.")
        None
      } else {
        Some(directory)
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) =>
        // выводим смску или результат юзеру
        println(s"Каталог недоступен: ${error.getMessage}")
        None
    }
  }

  // возвращаем файлы с расширением .store в выбранном каталоге
  private def listContainers(directory: Path): List[Path] = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // открываем поток списка файлов; после обхода его нужно закрыть
      val stream = Files.list(directory)
      // операции могут закончиться ошибкой; ниже catch обработает исключение
      try {
        stream.iterator().asScala
          // оставляем только элементы, подходящие под условие
          .filter(path => Files.isRegularFile(path))
          // оставляем только элементы, подходящие под условие
          .filter(path => path.getFileName.toString.toLowerCase.endsWith(".store"))
          // собираем результат в список List
          .toList
          .sortBy(_.getFileName.toString.toLowerCase)
      // этот блок выполняется и при успехе, и при ошибке; здесь освобождаем ресурсы
      } finally {
        // закрываем поток и освобождаем связанный с ним ресурс
        stream.close()
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(_) => Nil
    }
  }

  // закрываем прежнюю сессию и запоминаем новую; старый результат scan сбрасываем
  private def replaceCurrentStorage(session: StorageSession): Unit = {
    currentStorage.foreach(ContainerService.close)
    // запоминаем открытую сессию внутри Some
    currentStorage = Some(session)
    // старый результат проверки больше не используем
    lastScan = None
  }

  // проверяем, открыто ли хранилище, и только после этого выполняем переданное действие
  private def withStorage(action: StorageSession => Unit): Unit = {
    // проверяем Option: есть открытая сессия или её пока нет
    currentStorage match {
      // значение есть; извлекаем его из Some и используем в этой ветке
      case Some(session) => action(session)
      // значения нет: например, ничего не найдено или ответ не получен
      case None          => println("Сначала создайте или откройте хранилище.")
    }
  }

  // разбиваем строку по запятым, убираем пробелы и пустые правила
  private def parseExclusions(text: String): List[String] = {
    text.split(",").map(_.trim).filter(_.nonEmpty).toList
  }

  // проверяем, что пользователь ввёл одно имя файла, а не путь из нескольких частей
  private def isSimpleFileName(fileName: String): Boolean = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      fileName.nonEmpty && Paths.get(fileName).getNameCount == 1 &&
        Paths.get(fileName).getFileName.toString == fileName
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(_) => false
    }
  }

  // преобразуем Boolean в понятное пользователю да или нет
  private def yesNo(value: Boolean): String = if (value) "да" else "нет"
}

