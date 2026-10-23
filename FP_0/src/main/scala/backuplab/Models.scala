// здесь описаны данные приложения: настройки, источники, версии и результаты операций
// case class удобно использовать для хранения данных; copy создаёт копию с изменёнными полями
// String — текст, Int — целое число, Long — целое число для больших размеров и времени
// Boolean принимает только true или false: да или нет
// List[тип] — список элементов указанного типа; Array[Byte] — массив байтов
// Option[тип] — значение, которое может отсутствовать: Some(значение) или None
// val запрещает повторное присваивание, var разрешает менять значение переменной
// def объявляет метод; после двоеточия указан тип результата
// знак _ в короткой функции обозначает её текущий элемент

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.file.Path
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// настройки приложения из config.properties
case class AppConfig(
    // путь к файлу журнала
    logFile: String,
    // каталог для отдельных файлов ключей
    keyDirectory: String,
    // имя контейнера, предлагаемое при создании
    defaultContainerName: String,
    // нужно ли автоматически искать изменения после открытия
    checkOnStorageOpen: Boolean,
    // нужно ли подтверждение перед резервным копированием
    confirmBeforeBackup: Boolean,
    // нужно ли спрашивать перед заменой существующего файла
    confirmBeforeRestoreOverwrite: Boolean,
    // если подтверждение отключено, true разрешает замену, false означает пропуск
    overwriteExistingWithoutConfirmation: Boolean,
    // ограничение истории одного файла
    maximumVersionsPerFile: Int,
    // название алгоритма проверки содержимого
    hashAlgorithm: String,
    // стандартные исключения для новых источников
    defaultExclusions: List[String]
)

// один источник: путь, правила обхода, включение и дата копирования
case class BackupSource(
    // номер записи
    id: Int,
    // путь к исходному файлу или папке
    path: String,
    // нужно ли обходить вложенные папки
    includeSubdirectories: Boolean,
    // список правил исключения
    exclusions: List[String],
    // включён ли источник в проверку
    enabled: Boolean,
    // дата последней отмеченной успешной копии; None означает, что даты пока нет
    lastSuccessfulBackup: Option[String]
)

// сведения об одной операции резервного копирования
case class BackupVersion(
    // номер записи
    id: Int,
    // дата и время создания
    createdAt: String,
    // количество новых файлов
    added: Int,
    // количество изменённых файлов
    modified: Int,
    // количество удалённых файлов
    deleted: Int,
    // количество перемещённых файлов
    moved: Int,
    // количество пропущенных изменений или файлов
    skipped: Int,
    // объём реально добавленных данных в байтах
    writtenBytes: Long,
    // текстовый результат операции
    result: String,
    // необязательный комментарий пользователя
    comment: String
)

// состояние одного файла в определённой версии
case class FileRevision(
    // номер источника, которому принадлежит файл
    sourceId: Int,
    // путь файла внутри источника
    relativePath: String,
    // номер версии хранилища
    versionId: Int,
    // тип события: добавление, изменение, удаление или перемещение
    changeType: String,
    // размер файла в байтах
    size: Long,
    // время изменения в миллисекундах
    modifiedTime: Long,
    // сохранённый отпечаток содержимого файла
    hash: String,
    // относительный путь к сохранённым байтам внутри контейнера
    dataFile: String
)

// все метаданные хранилища; содержимое файлов хранится отдельно
case class StorageData(
    // название хранилища
    name: String,
    // дата и время создания
    createdAt: String,
    // включено ли шифрование
    encrypted: Boolean,
    // название алгоритма проверки содержимого
    hashAlgorithm: String,
    // список настроенных источников
    sources: List[BackupSource],
    // список резервных версий
    versions: List[BackupVersion],
    // история состояний отдельных файлов
    revisions: List[FileRevision],
    // номер, который получит следующий источник
    nextSourceId: Int,
    // номер следующей резервной версии
    nextVersionId: Int
)

// открытое хранилище: контейнер, рабочая папка, ключ и текущие метаданные
// поля val нельзя переприсвоить; data объявлено var, потому что состояние меняется
class StorageSession(
    // путь к переносимому файлу .store
    val containerPath: Path,
    // временная папка с распакованными данными
    val workDirectory: Path,
    // включено ли шифрование
    val encrypted: Boolean,
    // ключ шифрования; None означает отсутствие ключа
    val key: Option[Array[Byte]],
    // текущее содержимое метаданных сессии
    var data: StorageData
)

// одно изменение, найденное при проверке исходных файлов
case class FileChange(
    // тип события: добавление, изменение, удаление или перемещение
    changeType: String,
    // номер источника, которому принадлежит файл
    sourceId: Int,
    // путь файла внутри источника
    relativePath: String,
    // полный путь к исходному файлу
    absolutePath: String,
    // размер файла в байтах
    size: Long,
    // время изменения в миллисекундах
    modifiedTime: Long,
    // сохранённый отпечаток содержимого файла
    hash: String,
    // прежний путь при перемещении; для остальных событий обычно None
    previousRelativePath: Option[String]
)

// результат scan: изменения, недоступные источники и ошибки чтения
case class ScanResult(
    // список обнаруженных изменений
    changes: List[FileChange],
    // источники, которые не удалось проверить
    unavailableSources: List[String],
    // файлы, которые не удалось прочитать
    unreadableFiles: List[String]
) {
  // считаем изменения с типом ADDED
  def addedCount: Int = changes.count(_.changeType == "ADDED")
  // считаем изменения с типом MODIFIED
  def modifiedCount: Int = changes.count(_.changeType == "MODIFIED")
  // считаем изменения с типом DELETED
  def deletedCount: Int = changes.count(_.changeType == "DELETED")
  // считаем изменения с типом MOVED
  def movedCount: Int = changes.count(_.changeType == "MOVED")

  // суммируем размеры новых, изменённых и перемещённых файлов; удаления не добавляют данные
  def dataSize: Long = {
    changes
      // оставляем только элементы, подходящие под условие
      .filter(change => change.changeType != "DELETED")
      // преобразуем каждый элемент в новое значение; _ обозначает текущий элемент
      .map(_.size)
      // складываем все числа, полученные на предыдущем шаге
      .sum
  }
}

// счётчики, которые показываем после создания резервной версии
case class BackupResult(
    // номер версии хранилища
    versionId: Int,
    // количество новых файлов
    added: Int,
    // количество изменённых файлов
    modified: Int,
    // количество удалённых файлов
    deleted: Int,
    // количество перемещённых файлов
    moved: Int,
    // количество пропущенных изменений или файлов
    skipped: Int,
    // объём реально добавленных данных в байтах
    writtenBytes: Long
)

// результат восстановления: сколько файлов записали, пропустили и не смогли восстановить
case class RestoreResult(restored: Int, skipped: Int, failed: Int)

// результат проверки: число проверенных блоков и список проблем
case class VerifyResult(checked: Int, problems: List[String])

// общая функция даты и времени для версий и журнала
object TimeHelper {
  // правило записи даты и времени
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  // получаем текущее местное время компьютера и превращаем его в строку
  def now(): String = LocalDateTime.now().format(formatter)
}

