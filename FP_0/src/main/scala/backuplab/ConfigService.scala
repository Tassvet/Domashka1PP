// читаем config.properties, проверяем настройки и при первом запуске создаём стандартный файл
// при ошибке возвращаем Left со списком сообщений, при успехе — Right с AppConfig

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.Properties
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

object ConfigService {
  // начальные настройки; используются при создании config.properties
  val defaultConfig: AppConfig = AppConfig(
    logFile = "backup.log",
    keyDirectory = "keys",
    defaultContainerName = "backup.store",
    checkOnStorageOpen = true,
    confirmBeforeBackup = true,
    confirmBeforeRestoreOverwrite = true,
    overwriteExistingWithoutConfirmation = false,
    maximumVersionsPerFile = 10,
    hashAlgorithm = "SHA-256",
    defaultExclusions = List("*.tmp", "*.log", "cache", "node_modules")
  )

  // если файла настроек нет, создаём его; затем вызываем load для чтения и проверки
  def loadOrCreate(path: Path): Either[List[String], AppConfig] = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // обрабатываем отсутствие файла или папки
      if (!Files.exists(path)) {
        saveDefault(path)
      }
      load(path)
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(List(s"Не удалось подготовить конфигурацию: ${error.getMessage}"))
    }
  }

  // читаем конфигурацию и проверяем обязательные значения, числа, алгоритм и исключения
  // возвращаем Either: Left со списком ошибок либо Right с AppConfig
  def load(path: Path): Either[List[String], AppConfig] = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // находим строки, которые не похожи на запись имя=значение
      val malformedLines = Files
        .readAllLines(path, StandardCharsets.UTF_8)
        .asScala
        .zipWithIndex
        .filter { case (line, _) =>
          // получаем текст для дальнейшей обработки
          val text = line.trim
          text.nonEmpty && !text.startsWith("#") && !text.contains("=")
        }
        .map { case (_, index) => s"Строка ${index + 1} имеет неправильный формат." }
        // собираем результат в список List
        .toList

      // объект Properties хранит пары имя=значение
      val properties = new Properties()
      // Using закроет открытый ресурс, когда работа с ним закончится
      Using.resource(Files.newBufferedReader(path, StandardCharsets.UTF_8)) { reader =>
        properties.load(reader)
      }

      // сюда собираем сообщения об ошибках
      val errors = scala.collection.mutable.ListBuffer[String]()

      // читаем обязательную строку настройки; если она пустая, добавляем ошибку
      def required(name: String): String = {
        val value = Option(properties.getProperty(name)).map(_.trim).getOrElse("")
        // пустой ответ означает нажатие Enter без текста
        if (value.isEmpty) {
          errors += s"Параметр '$name' не указан."
        }
        value
      }

      // читаем настройку true/false; другие значения добавляем в список ошибок
      def boolean(name: String): Boolean = {
        required(name).toLowerCase match {
          // для команды true вызываем true
          case "true"  => true
          // для команды false вызываем false
          case "false" => false
          case _ =>
            errors += s"Параметр '$name' должен быть равен true или false."
            false
        }
      }

      // читаем путь журнала из настроек
      val logFile = required("logFile")
      // читаем каталог, в котором будем хранить ключи
      val keyDirectory = required("keyDirectory")
      // читаем имя контейнера по умолчанию
      val defaultContainerName = required("defaultContainerName")
      val checkOnStorageOpen = boolean("checkOnStorageOpen")
      val confirmBeforeBackup = boolean("confirmBeforeBackup")
      val confirmBeforeRestoreOverwrite = boolean("confirmBeforeRestoreOverwrite")
      val overwriteExisting = boolean("overwriteExistingWithoutConfirmation")

      // проверяем число сохраняемых версий; неверное число добавит ошибку
      val maximumVersions = required("maximumVersionsPerFile").toIntOption match {
        // значение есть; извлекаем его из Some и используем в этой ветке
        case Some(value) if value >= 1 => value
        case _ =>
          errors += "Параметр 'maximumVersionsPerFile' должен быть не меньше 1."
          1
      }

      // запоминаем алгоритм проверки содержимого
      val hashAlgorithm = required("hashAlgorithm").toUpperCase
      if (!Set("SHA-256", "SHA-512").contains(hashAlgorithm)) {
        errors += s"Алгоритм '$hashAlgorithm' не поддерживается."
      }

      // получаем список исключений
      val exclusions = required("defaultExclusions")
        .split(",")
        // преобразуем каждый элемент в новое значение; _ обозначает текущий элемент
        .map(_.trim)
        // оставляем только элементы, подходящие под условие
        .filter(_.nonEmpty)
        // собираем результат в список List
        .toList

      exclusions.foreach { rule =>
        if (rule.exists(character => "[]{}".contains(character))) {
          errors += s"Правило исключения '$rule' имеет неправильный формат."
        }
      }

      if (keyDirectory.nonEmpty) {
        // операции могут закончиться ошибкой; ниже catch обработает исключение
        try {
          // выбранный каталог
          val directory = Paths.get(keyDirectory)
          // создаём каталог и отсутствующие родительские папки
          Files.createDirectories(directory)
          if (!Files.isDirectory(directory) || !Files.isWritable(directory)) {
            errors += s"Каталог для ключей недоступен: $keyDirectory"
          }
        } catch {
          // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
          case NonFatal(_) => errors += s"Каталог для ключей невозможно создать: $keyDirectory"
        }
      }

      // объединяем ошибки формата и проверки значений
      val allErrors = malformedLines ++ errors.toList
      if (allErrors.nonEmpty) {
        Left(allErrors)
      } else {
        Right(
          AppConfig(
            logFile,
            keyDirectory,
            defaultContainerName,
            checkOnStorageOpen,
            confirmBeforeBackup,
            confirmBeforeRestoreOverwrite,
            overwriteExisting,
            maximumVersions,
            hashAlgorithm,
            exclusions
          )
        )
      }
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(List(s"Конфигурационный файл имеет неправильный формат: ${error.getMessage}"))
    }
  }

  // записываем начальные настройки в новый конфигурационный файл
  private def saveDefault(path: Path): Unit = {
    // получаем текст для дальнейшей обработки
    val text =
      """logFile=backup.log
        |keyDirectory=keys
        |defaultContainerName=backup.store
        |checkOnStorageOpen=true
        |confirmBeforeBackup=true
        |confirmBeforeRestoreOverwrite=true
        |overwriteExistingWithoutConfirmation=false
        |maximumVersionsPerFile=10
        |hashAlgorithm=SHA-256
        |defaultExclusions=*.tmp,*.log,cache,node_modules
        |""".stripMargin

    Option(path.toAbsolutePath.getParent).foreach(parent => Files.createDirectories(parent))
    // записываем данные; режим создания или замены указан в следующих аргументах
    Files.writeString(
      path,
      text,
      StandardCharsets.UTF_8,
      StandardOpenOption.CREATE_NEW
    )
  }
}

