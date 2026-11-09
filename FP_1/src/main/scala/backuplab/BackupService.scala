package backuplab

import java.nio.file.{Files, Path, StandardCopyOption}
import java.nio.file.attribute.BasicFileAttributes

object BackupService {
  // результат подготовки одного изменения: записи истории и объём новых блоков
  private case class Prepared(revisions: List[FileRevision], bytes: Long)

  def createBackup(session: StorageSession, scan: ScanResult, comment: Option[String],
                   config: AppConfig): Either[AppError, BackupResult] = {
    if (scan.changes.isEmpty) return Left(AppError.NoChanges)
    checkFreeSpace(session, scan).flatMap { _ =>
      val versionId = session.data.nextVersionId
      val results = scan.changes.map(change => prepare(session, change, versionId))
      val saved = results.flatMap(_.toOption)
      val problems = scan.problems ++ results.flatMap(_.left.toOption)
      val revisions = saved.flatMap(_.revisions)
      if (revisions.isEmpty) Left(AppError.NothingSaved(problems))
      else {
        val successfulChanges = scan.changes.zip(results).collect { case (change, Right(_)) => change }
        val added = successfulChanges.count(_.changeType == ChangeType.ADDED)
        val modified = successfulChanges.count(_.changeType == ChangeType.MODIFIED)
        val deleted = successfulChanges.count(_.changeType == ChangeType.DELETED)
        val moved = successfulChanges.count(_.changeType == ChangeType.MOVED)
        val bytes = saved.map(_.bytes).sum
        val version = BackupVersion(versionId, TimeHelper.now(), added, modified, deleted, moved,
          problems.size, bytes, if (problems.isEmpty) BackupStatus.Complete else BackupStatus.Partial, comment)
        val sourceIds = revisions.map(_.sourceId).toSet
        val sources = session.data.sources.map { source =>
          if (sourceIds.contains(source.id)) source.copy(lastSuccessfulBackup = Some(version.createdAt))
          else source
        }
        val old = session.data
        session.data = old.copy(sources = sources, versions = old.versions :+ version,
          revisions = keepRecent(old.revisions ++ revisions, config.maximumVersionsPerFile),
          nextVersionId = versionId + 1)
        ContainerService.save(session) match {
          case Left(error) =>
            session.data = old
            Left(error)
          case Right(_) =>
            // сначала фиксируем новую версию, потом удаляем блоки, на которые больше нет ссылок
            val cleanup = removeUnusedDataFiles(session).flatMap(_ => ContainerService.save(session))
            val allProblems = problems ++ cleanup.left.toOption.toList
            Right(BackupResult(versionId, added, modified, deleted, moved, problems.size, bytes, allProblems))
        }
      }
    }
  }

  private def prepare(session: StorageSession, change: FileChange, version: Int): Either[AppError, Prepared] = {
    change.changeType match {
      case ChangeType.DELETED => Right(Prepared(List(deletedRevision(change, version, change.relativePath)), 0L))
      case ChangeType.MOVED =>
        for {
          previous <- change.previousRelativePath.toRight(AppError.CorruptMetadata("старый путь перемещения"))
          saved <- saveFileRevision(session, change, version, ChangeType.ADDED)
        } yield Prepared(List(saved._1, deletedRevision(change, version, previous)), saved._2)
      case ChangeType.ADDED | ChangeType.MODIFIED =>
        saveFileRevision(session, change, version, change.changeType)
          .map { case (revision, bytes) => Prepared(List(revision), bytes) }
    }
  }

  private def saveFileRevision(session: StorageSession, change: FileChange, version: Int,
                               kind: ChangeType): Either[AppError, (FileRevision, Long)] = {
    for {
      sourceText <- change.absolutePath.toRight(AppError.CorruptMetadata("исходный путь"))
      source <- FileHelper.path(sourceText)
      _ <- if (Files.isRegularFile(source) && Files.isReadable(source)) Right(())
           else Left(AppError.FileUnavailable(sourceText))
      before <- FileHelper.attempt("проверка перед копированием", sourceText) {
        Files.readAttributes(source, classOf[BasicFileAttributes])
      }
      currentHash <- HashService.calculate(source, session.data.hashAlgorithm)
      _ <- if (before.size() == change.size && currentHash == change.hash) Right(())
           else Left(AppError.FileChanged(sourceText))
      relative = s"data/${change.hash}.bin"
      stored <- FileHelper.safePath(session.workDirectory, relative)
      valid <- if (Files.isRegularFile(stored)) {
        HashService.calculate(stored, session.data.hashAlgorithm).map(_ == change.hash)
      } else Right(false)
      bytes <- if (valid) Right(0L) else copyChecked(session, source, stored, change, before)
    } yield (FileRevision(change.sourceId, change.relativePath, version, kind, change.size,
      change.modifiedTime, change.hash, Some(relative)), bytes)
  }

  private def copyChecked(session: StorageSession, source: Path, stored: Path, change: FileChange,
                           before: BasicFileAttributes): Either[AppError, Long] = {
    FileHelper.attempt("подготовка блока", stored.toString) {
      Files.createDirectories(stored.getParent)
      Files.createTempFile(stored.getParent, "copy-", ".part")
    }.flatMap { temporary =>
      try {
        for {
          after <- FileHelper.attempt("копирование файла", source.toString) {
            Files.copy(source, temporary, StandardCopyOption.REPLACE_EXISTING)
            Files.readAttributes(source, classOf[BasicFileAttributes])
          }
          copiedHash <- HashService.calculate(temporary, session.data.hashAlgorithm)
          _ <- if (before.size() == after.size() && before.lastModifiedTime() == after.lastModifiedTime()
                    && copiedHash == change.hash) Right(())
               else Left(AppError.FileChanged(source.toString))
          _ <- FileHelper.attempt("сохранение блока", stored.toString) {
            Files.move(temporary, stored, StandardCopyOption.REPLACE_EXISTING)
          }
        } yield change.size
      } finally FileHelper.attempt("удаление временного блока", temporary.toString)(Files.deleteIfExists(temporary))
    }
  }

  private def deletedRevision(change: FileChange, version: Int, path: String): FileRevision =
    FileRevision(change.sourceId, path, version, ChangeType.DELETED, change.size, change.modifiedTime, change.hash, None)

  private def keepRecent(revisions: List[FileRevision], limit: Int): List[FileRevision] = {
    revisions.groupBy(r => (r.sourceId, r.relativePath)).values.flatMap { fileRevisions =>
      val sorted = fileRevisions.sortBy(_.versionId)
      val recent = sorted.takeRight(limit)
      sorted.filter(_.changeType != ChangeType.DELETED).lastOption match {
        case Some(content) if !recent.contains(content) && recent.lastOption.exists(_.changeType == ChangeType.DELETED) =>
          content :: recent
        case _ => recent
      }
    }.toList
  }

  private def removeUnusedDataFiles(session: StorageSession): Either[AppError, Unit] = {
    val used = session.data.revisions.flatMap(_.dataFile).toSet
    val directory = session.workDirectory.resolve("data")
    FileHelper.attempt("очистка старых блоков", directory.toString) {
      import scala.jdk.CollectionConverters.*
      val files = Files.list(directory)
      try files.iterator().asScala.filter(Files.isRegularFile(_))
        .filterNot(p => used.contains(s"data/${p.getFileName}")).foreach(Files.deleteIfExists(_))
      finally files.close()
    }
  }

  private def checkFreeSpace(session: StorageSession, scan: ScanResult): Either[AppError, Unit] = {
    FileHelper.attempt("проверка места", session.containerPath.toString) {
      val containerSize = if (Files.exists(session.containerPath)) Files.size(session.containerPath) else 0L
      val required = containerSize + scan.dataSize + 10L * 1024 * 1024
      val available = Files.getFileStore(session.containerPath.getParent).getUsableSpace
      val workAvailable = Files.getFileStore(session.workDirectory).getUsableSpace
      (required, available, workAvailable)
    }.flatMap { case (required, available, workAvailable) =>
      if (available < required) Left(AppError.NotEnoughSpace(required, available))
      else if (workAvailable < scan.dataSize) Left(AppError.NotEnoughSpace(scan.dataSize, workAvailable))
      else Right(())
    }
  }
}
