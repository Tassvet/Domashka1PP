package backuplab

import java.nio.file.Path
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class AppConfig(
    logFile: String, keyDirectory: String, defaultContainerName: String,
    checkOnStorageOpen: Boolean, confirmBeforeBackup: Boolean,
    confirmBeforeRestoreOverwrite: Boolean, overwriteExistingWithoutConfirmation: Boolean,
    maximumVersionsPerFile: Int, hashAlgorithm: String, defaultExclusions: List[String]
)

// эти имена совпадают с обозначениями в контейнерах ФП0
enum ChangeType {
  case ADDED, MODIFIED, DELETED, MOVED
}

enum BackupStatus {
  case Complete, Partial
}

enum SourceState {
  case Enabled, Disabled, Unavailable
}

case class BackupSource(
    id: Int, path: String, includeSubdirectories: Boolean, exclusions: List[String],
    enabled: Boolean, lastSuccessfulBackup: Option[String]
)

case class BackupVersion(
    id: Int, createdAt: String, added: Int, modified: Int, deleted: Int, moved: Int,
    skipped: Int, writtenBytes: Long, result: BackupStatus, comment: Option[String]
)

case class FileRevision(
    sourceId: Int, relativePath: String, versionId: Int, changeType: ChangeType,
    size: Long, modifiedTime: Long, hash: String, dataFile: Option[String]
)

case class StorageData(
    name: String, createdAt: String, encrypted: Boolean, hashAlgorithm: String,
    sources: List[BackupSource], versions: List[BackupVersion], revisions: List[FileRevision],
    nextSourceId: Int, nextVersionId: Int
)

class StorageSession(
    val containerPath: Path, val workDirectory: Path, val encrypted: Boolean,
    val key: Option[Array[Byte]], var data: StorageData
)

case class FileChange(
    changeType: ChangeType, sourceId: Int, relativePath: String, absolutePath: Option[String],
    size: Long, modifiedTime: Long, hash: String, previousRelativePath: Option[String]
)

case class ScanResult(changes: List[FileChange], problems: List[AppError]) {
  def addedCount: Int = changes.count(_.changeType == ChangeType.ADDED)
  def modifiedCount: Int = changes.count(_.changeType == ChangeType.MODIFIED)
  def deletedCount: Int = changes.count(_.changeType == ChangeType.DELETED)
  def movedCount: Int = changes.count(_.changeType == ChangeType.MOVED)
  def dataSize: Long = changes.filter(_.changeType != ChangeType.DELETED).map(_.size).sum
}

case class BackupResult(
    versionId: Int, added: Int, modified: Int, deleted: Int, moved: Int,
    skipped: Int, writtenBytes: Long, problems: List[AppError]
)

// сервис составляет план, а интерфейс собирает подтверждения до начала восстановления
case class RestoreItem(revision: FileRevision, target: Path)
case class RestorePlan(versionId: Int, destination: Path, items: List[RestoreItem])
enum RestoreOutcome {
  case Restored, Skipped
}

case class RestoreResult(restored: Int, skipped: Int, problems: List[AppError]) {
  def failed: Int = problems.size
}
case class VerifyResult(checked: Int, problems: List[AppError])
case class CollectedFiles(files: List[Path], problems: List[AppError])

object TimeHelper {
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
  def now(): String = LocalDateTime.now().format(formatter)
}
