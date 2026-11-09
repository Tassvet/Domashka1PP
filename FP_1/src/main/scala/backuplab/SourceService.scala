package backuplab

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable.ListBuffer

object SourceService {
  def addSource(session: StorageSession, pathText: String, includeSubdirectories: Boolean,
                exclusions: List[String]): Either[AppError, BackupSource] = {
    for {
      path <- checkedPath(session, pathText, None)
      _ <- validateExclusions(exclusions)
      source = BackupSource(session.data.nextSourceId, path.toString, includeSubdirectories,
        exclusions, true, None)
      _ <- saveChange(session, session.data.copy(
        sources = session.data.sources :+ source, nextSourceId = source.id + 1))
    } yield source
  }

  def editSource(session: StorageSession, id: Int, pathText: String, include: Boolean,
                 exclusions: List[String]): Either[AppError, Unit] = {
    for {
      old <- findSource(session, id)
      path <- checkedPath(session, pathText, Some(id))
      _ <- validateExclusions(exclusions)
      changed = old.copy(path = path.toString, includeSubdirectories = include, exclusions = exclusions)
      sources = session.data.sources.map(source => if (source.id == id) changed else source)
      _ <- saveChange(session, session.data.copy(sources = sources))
    } yield ()
  }

  def findSource(session: StorageSession, id: Int): Either[AppError, BackupSource] = {
    session.data.sources.find(_.id == id).toRight(AppError.SourceNotFound(id))
  }

  def removeSource(session: StorageSession, id: Int): Either[AppError, Unit] = {
    findSource(session, id).flatMap { _ =>
      saveChange(session, session.data.copy(sources = session.data.sources.filterNot(_.id == id)))
    }
  }

  def toggleSource(session: StorageSession, id: Int): Either[AppError, Boolean] = {
    findSource(session, id).flatMap { source =>
      val enabled = !source.enabled
      val sources = session.data.sources.map(item => if (item.id == id) item.copy(enabled = enabled) else item)
      saveChange(session, session.data.copy(sources = sources)).map(_ => enabled)
    }
  }

  def state(source: BackupSource): SourceState = {
    if (!source.enabled) SourceState.Disabled
    else if (Files.isReadable(Paths.get(source.path))) SourceState.Enabled
    else SourceState.Unavailable
  }

  def collectFiles(source: BackupSource): Either[AppError, CollectedFiles] = {
    val root = Paths.get(source.path)
    if (!Files.exists(root) || !Files.isReadable(root)) return Left(AppError.SourceUnavailable(source.path))
    if (Files.isRegularFile(root)) return Right(CollectedFiles(List(root), Nil))
    if (!Files.isDirectory(root)) return Left(AppError.SourceUnavailable(source.path))

    // обход Java вызывает обработчики по одному, поэтому здесь удобен ListBuffer
    val files = ListBuffer[Path]()
    val problems = ListBuffer[AppError]()
    FileHelper.attempt("обход источника", source.path) {
      Files.walkFileTree(root, java.util.Collections.emptySet[java.nio.file.FileVisitOption](),
        if (source.includeSubdirectories) Int.MaxValue else 1,
        new SimpleFileVisitor[Path]() {
          override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (dir != root && isExcluded(root.relativize(dir), source.exclusions)) FileVisitResult.SKIP_SUBTREE
            else FileVisitResult.CONTINUE
          }
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (attrs.isRegularFile && !isExcluded(root.relativize(file), source.exclusions)) files += file
            FileVisitResult.CONTINUE
          }
          override def visitFileFailed(file: Path, error: java.io.IOException): FileVisitResult = {
            problems += AppError.FileUnavailable(file.toString)
            FileVisitResult.CONTINUE
          }
        })
      CollectedFiles(files.toList, problems.toList)
    }
  }

  def relativePath(source: BackupSource, file: Path): String = {
    val root = Paths.get(source.path)
    val relative = if (Files.isRegularFile(root)) root.getFileName else root.relativize(file)
    relative.toString.replace('\\', '/')
  }

  // исключённый настройками файл не считается удалённым с диска
  def isTracked(source: BackupSource, relative: String): Boolean = {
    val path = Paths.get(relative)
    !isExcluded(path, source.exclusions) && (source.includeSubdirectories || path.getNameCount == 1)
  }

  def validateExclusions(rules: List[String]): Either[AppError, Unit] = {
    rules.find(_.exists("[]{}".contains(_))) match {
      case Some(rule) => Left(AppError.InvalidSetting("исключения", rule))
      case None => Right(())
    }
  }

  private def checkedPath(session: StorageSession, text: String, currentId: Option[Int]): Either[AppError, Path] = {
    FileHelper.path(text).flatMap { path =>
      if (!Files.isRegularFile(path) && !Files.isDirectory(path)) Left(AppError.SourceUnavailable(text))
      else if (session.data.sources.exists(s => !currentId.contains(s.id) && Paths.get(s.path) == path))
        Left(AppError.DuplicateSource(text))
      else Right(path)
    }
  }

  private def isExcluded(path: Path, rules: List[String]): Boolean = {
    import scala.jdk.CollectionConverters.*
    val names = path.iterator().asScala.map(_.toString.toLowerCase).toList
    val full = path.toString.replace('\\', '/').toLowerCase
    rules.exists { rule =>
      val regex = globToRegex(rule.toLowerCase)
      names.exists(_.matches(regex)) || full.matches(regex)
    }
  }

  private def globToRegex(rule: String): String = {
    "^" + rule.map {
      case '*' => ".*"
      case '?' => "."
      case c if "\\.^$|()[]{}+".contains(c) => "\\" + c
      case c => c.toString
    }.mkString + "$"
  }

  private def saveChange(session: StorageSession, data: StorageData): Either[AppError, Unit] = {
    val old = session.data
    session.data = data
    ContainerService.save(session).left.map { error =>
      session.data = old
      error
    }
  }
}
