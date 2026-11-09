package backuplab

import java.nio.file.Files

object VerifyService {
  def verify(session: StorageSession): VerifyResult = {
    val ids = session.data.versions.map(_.id)
    val duplicateProblems = if (ids.distinct.size == ids.size) Nil else List(AppError.DuplicateVersions)
    val referenceProblems = session.data.revisions.filterNot(r => ids.contains(r.versionId))
      .map(r => AppError.UnknownVersion(r.relativePath, r.versionId))
    val blocks = session.data.revisions.flatMap(r => r.dataFile.map(_ -> r)).groupBy(_._1)
    // каждый блок читается один раз, но проверяются хеши всех записей, которые на него ссылаются
    val blockProblems = blocks.toList.flatMap { case (relative, records) =>
      val checked = for {
        path <- FileHelper.safePath(session.workDirectory, relative)
        _ <- if (Files.isRegularFile(path)) Right(()) else Left(AppError.MissingData(relative))
        hash <- HashService.calculate(path, session.data.hashAlgorithm)
        _ <- if (records.forall(_._2.hash == hash)) Right(()) else Left(AppError.HashMismatch(relative))
      } yield ()
      checked.left.toOption
    }
    val missingProblems = session.data.revisions.filter(r => r.changeType != ChangeType.DELETED && r.dataFile.isEmpty)
      .map(r => AppError.MissingData(r.relativePath))
    VerifyResult(blocks.size, (duplicateProblems ++ referenceProblems ++ blockProblems ++ missingProblems).distinct)
  }
}
