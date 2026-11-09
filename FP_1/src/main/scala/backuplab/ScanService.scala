package backuplab

import java.nio.file.{Files, Path}
import java.nio.file.attribute.BasicFileAttributes

object ScanService {
  def scan(session: StorageSession): ScanResult = {
    val latest = latestRevisions(session.data.revisions)
    val results = session.data.sources.filter(_.enabled).map { source =>
      scanSource(source, latest, session.data.hashAlgorithm)
    }
    ScanResult(results.flatMap(_.changes), results.flatMap(_.problems))
  }

  private def scanSource(source: BackupSource, latest: Map[(Int, String), FileRevision],
                         algorithm: String): ScanResult = {
    SourceService.collectFiles(source) match {
      case Left(error) => ScanResult(Nil, List(error))
      case Right(collected) =>
        val inspected = collected.files.map(file => inspect(source, file, latest, algorithm))
        val found = inspected.flatMap(_.toOption).flatten
        val errors = collected.problems ++ inspected.flatMap(_.left.toOption)
        val seen = collected.files.map(file => SourceService.relativePath(source, file)).toSet
        val deleted = if (collected.problems.nonEmpty) Nil else {
          latest.values.filter(r => r.sourceId == source.id && r.changeType != ChangeType.DELETED)
            .filter(r => SourceService.isTracked(source, r.relativePath))
            .filterNot(r => seen.contains(r.relativePath)).map { r =>
              FileChange(ChangeType.DELETED, source.id, r.relativePath, None,
                r.size, r.modifiedTime, r.hash, None)
            }.toList
        }
        ScanResult(detectMoves(found ++ deleted), errors)
    }
  }

  private def inspect(source: BackupSource, path: Path, latest: Map[(Int, String), FileRevision],
                      algorithm: String): Either[AppError, Option[FileChange]] = {
    val relative = SourceService.relativePath(source, path)
    val snapshot = for {
      before <- FileHelper.attempt("свойства файла", path.toString) {
        Files.readAttributes(path, classOf[BasicFileAttributes])
      }
      hash <- HashService.calculate(path, algorithm)
      after <- FileHelper.attempt("свойства файла", path.toString) {
        Files.readAttributes(path, classOf[BasicFileAttributes])
      }
    } yield (before, after, hash)
    snapshot.flatMap { case (before, after, hash) =>
      if (before.size() != after.size() || before.lastModifiedTime() != after.lastModifiedTime())
        Left(AppError.FileChanged(path.toString))
      else {
        val change = latest.get((source.id, relative)) match {
          case None => Some(ChangeType.ADDED)
          case Some(old) if old.changeType == ChangeType.DELETED => Some(ChangeType.ADDED)
          case Some(old) if old.hash != hash => Some(ChangeType.MODIFIED)
          case _ => None
        }
        Right(change.map(kind => FileChange(kind, source.id, relative, Some(path.toString),
          after.size(), after.lastModifiedTime().toMillis, hash, None)))
      }
    }
  }

  def latestRevisions(revisions: List[FileRevision]): Map[(Int, String), FileRevision] = {
    revisions.groupBy(r => (r.sourceId, r.relativePath)).view.mapValues(_.maxBy(_.versionId)).toMap
  }

  private def detectMoves(changes: List[FileChange]): List[FileChange] = {
    val added = changes.filter(_.changeType == ChangeType.ADDED)
    val deleted = changes.filter(_.changeType == ChangeType.DELETED)
    // остаток удалённых файлов не даёт связать один старый файл с двумя новыми
    val (remaining, replacements) = added.foldLeft((deleted, List.empty[FileChange])) {
      case ((unused, result), newFile) =>
        unused.find(old => old.sourceId == newFile.sourceId && old.hash == newFile.hash && old.size == newFile.size) match {
          case None => (unused, newFile :: result)
          case Some(old) =>
            val moved = newFile.copy(changeType = ChangeType.MOVED, previousRelativePath = Some(old.relativePath))
            (unused.filterNot(_ == old), moved :: result)
        }
    }
    changes.filter(c => c.changeType != ChangeType.ADDED && c.changeType != ChangeType.DELETED) ++
      remaining ++ replacements.reverse
  }
}
