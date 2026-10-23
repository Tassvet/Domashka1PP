// записываем и читаем сведения о хранилище во временной рабочей папке
// данные файлов лежат отдельно в data; здесь сохраняются настройки, источники и история

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.{Base64, Properties}
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

object StorageDataService {
  // служебный разделитель элементов списка исключений
  private val separator = "\u001f"

  // сохраняем свойства, источники, версии и историю в отдельные служебные файлы
  // ContainerService потом упакует эти файлы вместе с данными
  def save(workDirectory: Path, data: StorageData): Unit = {
    // создаём каталог и отсутствующие родительские папки
    Files.createDirectories(workDirectory)
    // создаём каталог и отсутствующие родительские папки
    Files.createDirectories(workDirectory.resolve("data"))

    // объект Properties хранит пары имя=значение
    val properties = new Properties()
    // сохраняем одно свойство: название параметра и его строковое значение
    properties.setProperty("name", data.name)
    // сохраняем одно свойство: название параметра и его строковое значение
    properties.setProperty("createdAt", data.createdAt)
    // сохраняем одно свойство: название параметра и его строковое значение
    properties.setProperty("encrypted", data.encrypted.toString)
    // сохраняем одно свойство: название параметра и его строковое значение
    properties.setProperty("hashAlgorithm", data.hashAlgorithm)
    // сохраняем одно свойство: название параметра и его строковое значение
    properties.setProperty("nextSourceId", data.nextSourceId.toString)
    // сохраняем одно свойство: название параметра и его строковое значение
    properties.setProperty("nextVersionId", data.nextVersionId.toString)

    // Using закроет открытый ресурс, когда работа с ним закончится
    Using.resource(Files.newBufferedWriter(
      workDirectory.resolve("storage.properties"),
      StandardCharsets.UTF_8,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )) { writer =>
      properties.store(writer, "BackupLab storage")
    }

    // превращаем источники в строки таблицы
    val sourceLines = data.sources.sortBy(_.id).map { source =>
      List(
        source.id.toString,
        encode(source.path),
        source.includeSubdirectories.toString,
        encode(source.exclusions.mkString(separator)),
        source.enabled.toString,
        encode(source.lastSuccessfulBackup.getOrElse(""))
      ).mkString("\t")
    }

    // превращаем версии в строки таблицы
    val versionLines = data.versions.sortBy(_.id).map { version =>
      List(
        version.id.toString,
        encode(version.createdAt),
        version.added.toString,
        version.modified.toString,
        version.deleted.toString,
        version.moved.toString,
        version.skipped.toString,
        version.writtenBytes.toString,
        encode(version.result),
        encode(version.comment)
      ).mkString("\t")
    }

    // превращаем историю файлов в строки таблицы
    val revisionLines = data.revisions
      .sortBy(revision => (revision.versionId, revision.sourceId, revision.relativePath))
      .map { revision =>
        List(
          revision.sourceId.toString,
          encode(revision.relativePath),
          revision.versionId.toString,
          revision.changeType,
          revision.size.toString,
          revision.modifiedTime.toString,
          revision.hash,
          encode(revision.dataFile)
        ).mkString("\t")
      }

    writeLines(workDirectory.resolve("sources.tsv"), sourceLines)
    writeLines(workDirectory.resolve("versions.tsv"), versionLines)
    writeLines(workDirectory.resolve("revisions.tsv"), revisionLines)
  }

  // читаем свойства и таблицы из рабочей папки и собираем StorageData
  // если служебные данные не удалось разобрать, возвращаем Left
  def load(workDirectory: Path): Either[String, StorageData] = {
    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // путь к свойствам хранилища
      val propertiesPath = workDirectory.resolve("storage.properties")
      // проверяем, что по пути есть обычный файл
      if (!Files.isRegularFile(propertiesPath)) {
        // прерываем этот метод и возвращаем ошибку вызывающему коду
        return Left("В контейнере отсутствует файл storage.properties.")
      }

      // объект Properties хранит пары имя=значение
      val properties = new Properties()
      // Using закроет открытый ресурс, когда работа с ним закончится
      Using.resource(Files.newBufferedReader(propertiesPath, StandardCharsets.UTF_8)) { reader =>
        properties.load(reader)
      }

      val name = required(properties, "name")
      val createdAt = required(properties, "createdAt")
      // признак включённого шифрования
      val encrypted = required(properties, "encrypted").toBoolean
      // запоминаем алгоритм проверки содержимого
      val hashAlgorithm = required(properties, "hashAlgorithm")
      // читаем следующий свободный номер источника
      val nextSourceId = required(properties, "nextSourceId").toInt
      // читаем номер следующей версии
      val nextVersionId = required(properties, "nextVersionId").toInt

      for {
        sources <- loadSources(workDirectory.resolve("sources.tsv"))
        versions <- loadVersions(workDirectory.resolve("versions.tsv"))
        revisions <- loadRevisions(workDirectory.resolve("revisions.tsv"))
      } yield StorageData(
        name,
        createdAt,
        encrypted,
        hashAlgorithm,
        sources,
        versions,
        revisions,
        nextSourceId,
        nextVersionId
      )
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(s"Метаданные хранилища повреждены: ${error.getMessage}")
    }
  }

  // читаем строки sources.tsv и создаём BackupSource для каждой строки
  private def loadSources(path: Path): Either[String, List[BackupSource]] = {
    parseLines(path, 6) { fields =>
      // декодируем сохранённые правила исключения
      val exclusionsText = decode(fields(3))
      // получаем список исключений
      val exclusions = if (exclusionsText.isEmpty) Nil else exclusionsText.split(separator, -1).toList
      // пустую дату превращаем в None, указанную — в Some
      val lastBackup = decode(fields(5)) match {
        case ""    => None
        case value => Some(value)
      }

      BackupSource(
        fields(0).toInt,
        decode(fields(1)),
        fields(2).toBoolean,
        exclusions,
        fields(4).toBoolean,
        lastBackup
      )
    }
  }

  // читаем versions.tsv и создаём список BackupVersion
  private def loadVersions(path: Path): Either[String, List[BackupVersion]] = {
    parseLines(path, 10) { fields =>
      BackupVersion(
        fields(0).toInt,
        decode(fields(1)),
        fields(2).toInt,
        fields(3).toInt,
        fields(4).toInt,
        fields(5).toInt,
        fields(6).toInt,
        fields(7).toLong,
        decode(fields(8)),
        decode(fields(9))
      )
    }
  }

  // читаем revisions.tsv и создаём записи истории FileRevision
  private def loadRevisions(path: Path): Either[String, List[FileRevision]] = {
    parseLines(path, 8) { fields =>
      FileRevision(
        fields(0).toInt,
        decode(fields(1)),
        fields(2).toInt,
        fields(3),
        fields(4).toLong,
        fields(5).toLong,
        fields(6),
        decode(fields(7))
      )
    }
  }

  // общий разбор таблицы: проверяем число полей и применяем переданную функцию create
  // ошибки формата возвращаем через Left
  private def parseLines[A](path: Path, expectedFields: Int)(create: Array[String] => A): Either[String, List[A]] = {
    // обрабатываем отсутствие файла или папки
    if (!Files.exists(path)) {
      // возвращаем успешный результат, дальнейшие строки метода не выполняются
      return Right(Nil)
    }

    // операции могут закончиться ошибкой; ниже catch обработает исключение
    try {
      // результат обработки
      val result = Files
        .readAllLines(path, StandardCharsets.UTF_8)
        .asScala
        .zipWithIndex
        // оставляем только элементы, подходящие под условие
        .filter(_._1.nonEmpty)
        .map { case (line, index) =>
          // делим строку таблицы на поля по табуляции
          val fields = line.split("\t", -1)
          if (fields.length != expectedFields) {
            throw new IllegalArgumentException(s"неправильная строка ${index + 1} в файле ${path.getFileName}")
          }
          create(fields)
        }
        // собираем результат в список List
        .toList

      Right(result)
    } catch {
      // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
      case NonFatal(error) => Left(s"Не удалось прочитать ${path.getFileName}: ${error.getMessage}")
    }
  }

  // получаем обязательное свойство; отсутствие значения означает ошибку
  private def required(properties: Properties, name: String): String = {
    Option(properties.getProperty(name)).map(_.trim).filter(_.nonEmpty).getOrElse {
      throw new IllegalArgumentException(s"параметр '$name' не указан")
    }
  }

  // записываем строки таблицы в UTF-8, заменяя предыдущий файл метаданных
  private def writeLines(path: Path, lines: List[String]): Unit = {
    // записываем данные; режим создания или замены указан в следующих аргументах
    Files.write(
      path,
      lines.asJava,
      StandardCharsets.UTF_8,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
  }

  // кодируем текст в Base64, чтобы табуляции и другие символы не ломали строки таблицы
  // Base64 — способ записи текста, а не шифрование
  private def encode(text: String): String = {
    Base64.getEncoder.encodeToString(text.getBytes(StandardCharsets.UTF_8))
  }

  // восстанавливаем исходную строку из Base64
  private def decode(text: String): String = {
    new String(Base64.getDecoder.decode(text), StandardCharsets.UTF_8)
  }
}

