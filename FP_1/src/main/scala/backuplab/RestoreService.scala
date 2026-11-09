package backuplab

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.nio.file.attribute.FileTime

object RestoreService {
  // план содержит только данные: никаких вопросов пользователю здесь нет
  def plan(session: StorageSession, versionId: Int, destination: Path): Either[AppError, RestorePlan] = {
    if (!session.data.versions.exists(_.id == versionId)) return Left(AppError.VersionNotFound(versionId))
    val root = destination.toAbsolutePath.normalize()
    val state = session.data.revisions.filter(_.versionId <= versionId)
      .groupBy(r => (r.sourceId, r.relativePath)).values.map(_.maxBy(_.versionId))
      .filter(_.changeType != ChangeType.DELETED).toList.sortBy(r => (r.sourceId, r.relativePath))
    state.foldLeft[Either[AppError, List[RestoreItem]]](Right(Nil)) { (result, revision) =>
      for {
        previous <- result
        folder <- FileHelper.safePath(root, sourceFolderName(session, revision.sourceId))
        target <- FileHelper.safePath(folder, revision.relativePath)
      } yield RestoreItem(revision, target) :: previous
    }.map(items => RestorePlan(versionId, root, items.reverse))
  }

  def restore(session: StorageSession, plan: RestorePlan,
              allowedOverwrites: Set[Path]): Either[AppError, RestoreResult] = {
    FileHelper.attempt("создание папки восстановления", plan.destination.toString) {
      Files.createDirectories(plan.destination)
    }.flatMap { _ =>
      if (!Files.isWritable(plan.destination)) Left(AppError.DestinationUnavailable(plan.destination.toString))
      else {
        val results = plan.items.map { item =>
          if (Files.exists(item.target) && !allowedOverwrites.contains(item.target)) Right(RestoreOutcome.Skipped)
          else restoreOneFile(session, item, plan.destination).map(_ => RestoreOutcome.Restored)
        }
        Right(RestoreResult(results.count(_ == Right(RestoreOutcome.Restored)), results.count(_ == Right(RestoreOutcome.Skipped)),
          results.flatMap(_.left.toOption)))
      }
    }
  }

  private def restoreOneFile(session: StorageSession, item: RestoreItem, root: Path): Either[AppError, Unit] = {
    val revision = item.revision
    for {
      target <- FileHelper.safePath(root, root.relativize(item.target).toString)
      relative <- revision.dataFile.toRight(AppError.MissingData(revision.relativePath))
      stored <- FileHelper.safePath(session.workDirectory, relative)
      _ <- if (Files.isRegularFile(stored)) Right(()) else Left(AppError.MissingData(revision.relativePath))
      hash <- HashService.calculate(stored, session.data.hashAlgorithm)
      _ <- if (hash == revision.hash) Right(()) else Left(AppError.HashMismatch(revision.relativePath))
      temporary <- FileHelper.attempt("подготовка восстановления", target.toString) {
        Files.createDirectories(target.getParent)
        Files.createTempFile(target.getParent, "restore-", ".part")
      }
      _ <- try {
        for {
          _ <- FileHelper.attempt("восстановление файла", target.toString) {
            Files.copy(stored, temporary, StandardCopyOption.REPLACE_EXISTING)
          }
          copiedHash <- HashService.calculate(temporary, session.data.hashAlgorithm)
          _ <- if (copiedHash == revision.hash) Right(()) else Left(AppError.HashMismatch(revision.relativePath))
          _ <- FileHelper.attempt("завершение восстановления", target.toString) {
            Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING)
            Files.setLastModifiedTime(target, FileTime.fromMillis(revision.modifiedTime))
          }
        } yield ()
      } finally FileHelper.attempt("удаление временного файла", temporary.toString)(Files.deleteIfExists(temporary))
    } yield ()
  }

  private def sourceFolderName(session: StorageSession, sourceId: Int): String = {
    val name = session.data.sources.find(_.id == sourceId)
      .flatMap(source => FileHelper.path(source.path).toOption.flatMap(path => Option(path.getFileName)))
      .map(_.toString).filter(_.nonEmpty).getOrElse("source")
    s"${sourceId}_${name.replaceAll("[^a-zA-Zа-яА-Я0-9._-]", "_")}"
  }
}
