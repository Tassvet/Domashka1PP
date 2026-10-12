// проверяем связи в метаданных и хеши сохранённых файлов
// проверка сообщает о проблемах, но сама не исправляет повреждённое хранилище

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.file.Files
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

object VerifyService {
  // проверяем номера версий, ссылки на них, наличие файлов данных и их хеши
  // возвращаем число проверенных блоков и сообщения о проблемах
  def verify(
      session: StorageSession,
      // настройки программы
      config: AppConfig,
      // журнал для записи событий
      log: LogService
  ): VerifyResult = {
    log.info("Начата проверка целостности")
    // сюда собираем результаты проверки с ошибками
    val problems = ListBuffer[String]()

    // номера всех сохранённых версий
    val versionIds = session.data.versions.map(_.id)
    if (versionIds.distinct.size != versionIds.size) {
      problems += "Обнаружены повторяющиеся номера версий."
    }

    // множество номеров для проверки ссылок из истории
    val knownVersions = versionIds.toSet
    session.data.revisions.foreach { revision =>
      if (!knownVersions.contains(revision.versionId)) {
        problems += s"Файл ${revision.relativePath} ссылается на неизвестную версию ${revision.versionId}."
      }
    }

    // проверяем каждый блок данных один раз
    val storedFiles = session.data.revisions
      // оставляем только элементы, подходящие под условие
      .filter(_.dataFile.nonEmpty)
      // объединяем записи в группы по указанному признаку
      .groupBy(_.dataFile)
      .values
      // преобразуем каждый элемент в новое значение; _ обозначает текущий элемент
      .map(_.head)
      // собираем результат в список List
      .toList

    storedFiles.foreach { revision =>
      // операции могут закончиться ошибкой; ниже catch обработает исключение
      try {
        val path = session.workDirectory.resolve(revision.dataFile).normalize()

        if (!path.startsWith(session.workDirectory)) {
          problems += s"Небезопасный путь в метаданных: ${revision.dataFile}"
        } else if (!Files.isRegularFile(path)) {
          problems += s"Отсутствуют данные файла: ${revision.relativePath}"
        } else {
          // хеш, вычисленный по сохранённым байтам
          val actualHash = HashService.calculate(path, session.data.hashAlgorithm)
          if (actualHash != revision.hash) {
            problems += s"Повреждены данные файла: ${revision.relativePath}"
          }
        }
      } catch {
        // обрабатываем обычную ошибку выполнения; дальнейшее действие зависит от метода
        case NonFatal(_) => problems += s"Не удалось проверить файл: ${revision.relativePath}"
      }
    }

    // результат обработки
    val result = VerifyResult(storedFiles.size, problems.distinct.toList)
    if (result.problems.isEmpty) {
      log.info(s"Проверка целостности завершена: проверено ${result.checked}, ошибок нет")
    } else {
      log.warning(s"Проверка целостности завершена: обнаружено проблем ${result.problems.size}")
    }
    result
  }
}

