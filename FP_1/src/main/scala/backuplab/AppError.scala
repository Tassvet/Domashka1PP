package backuplab

// у каждой ожидаемой ошибки свой вариант, а текст для консоли находится отдельно
enum AppError {
  case UnknownCommand(input: String)
  case UnknownSourceCommand(input: String)
  case InputClosed
  case Cancelled
  case StorageNotOpen
  case SourceNotFound(id: Int)
  case SourceUnavailable(path: String)
  case DuplicateSource(path: String)
  case FileUnavailable(path: String)
  case FileChanged(path: String)
  case ContainerNotFound(path: String)
  case InvalidContainer
  case UnsupportedFormat(version: Int)
  case CorruptMetadata(file: String)
  case MissingSetting(name: String)
  case InvalidSetting(name: String, value: String)
  case InvalidConfig(errors: List[AppError])
  case InvalidPath(path: String)
  case UnsafePath(path: String)
  case DestinationUnavailable(path: String)
  case KeyRequired
  case InvalidKey
  case CannotDecrypt
  case NoChanges
  case NoVersions
  case NothingSaved(errors: List[AppError])
  case NotEnoughSpace(required: Long, available: Long)
  case VersionNotFound(id: Int)
  case InvalidNumber(input: String)
  case MissingData(path: String)
  case HashMismatch(path: String)
  case DuplicateVersions
  case UnknownVersion(path: String, id: Int)
  case IoFailure(operation: String, path: String, reason: String)
}

// сервисы возвращают ошибку, а интерфейс выбирает, как её показать
object ErrorMessages {
  def text(error: AppError): String = error match {
    case AppError.UnknownCommand(input) => s"Неизвестная команда: $input. Введите help."
    case AppError.UnknownSourceCommand(input) => s"Неизвестная команда источников: $input."
    case AppError.InputClosed => "Ввод завершён."
    case AppError.Cancelled => "Операция отменена."
    case AppError.StorageNotOpen => "Сначала создайте или откройте хранилище."
    case AppError.SourceNotFound(id) => s"Источник $id не найден."
    case AppError.SourceUnavailable(path) => s"Источник недоступен: $path"
    case AppError.DuplicateSource(path) => s"Источник уже добавлен: $path"
    case AppError.FileUnavailable(path) => s"Файл недоступен: $path"
    case AppError.FileChanged(path) => s"Файл изменился во время чтения: $path"
    case AppError.ContainerNotFound(path) => s"Хранилище не найдено: $path"
    case AppError.InvalidContainer => "Это не контейнер BackupLab или его заголовок повреждён."
    case AppError.UnsupportedFormat(version) => s"Версия формата $version не поддерживается."
    case AppError.CorruptMetadata(file) => s"Повреждены метаданные: $file"
    case AppError.MissingSetting(name) => s"Не указан параметр: $name"
    case AppError.InvalidSetting(name, _) => s"Неправильное значение параметра: $name"
    case AppError.InvalidConfig(errors) => errors.map(text).mkString("Ошибки конфигурации:\n- ", "\n- ", "")
    case AppError.InvalidPath(path) => s"Неправильный путь: $path"
    case AppError.UnsafePath(path) => s"Небезопасный путь: $path"
    case AppError.DestinationUnavailable(path) => s"Каталог недоступен для записи: $path"
    case AppError.KeyRequired => "Для этого хранилища нужен ключ."
    case AppError.InvalidKey => "Ключ имеет неправильный формат или длину."
    case AppError.CannotDecrypt => "Не удалось расшифровать хранилище: неверный ключ или повреждены данные."
    case AppError.NoVersions => "Сохранённых версий нет."
    case AppError.NoChanges => "Изменений для сохранения нет."
    case AppError.NothingSaved(errors) => errors.map(text).mkString("Ни одно изменение не сохранено:\n- ", "\n- ", "")
    case AppError.NotEnoughSpace(required, available) =>
      s"Недостаточно места: требуется $required байт, доступно $available."
    case AppError.VersionNotFound(id) => s"Версия $id не найдена."
    case AppError.InvalidNumber(input) => s"Неправильный номер: $input"
    case AppError.MissingData(path) => s"Нет сохранённых данных: $path"
    case AppError.HashMismatch(path) => s"Не совпадает хеш файла: $path"
    case AppError.DuplicateVersions => "Обнаружены повторяющиеся номера версий."
    case AppError.UnknownVersion(path, id) => s"Файл $path ссылается на неизвестную версию $id."
    case AppError.IoFailure(operation, path, reason) => s"Не удалось выполнить: $operation ($path, $reason)."
  }
}
