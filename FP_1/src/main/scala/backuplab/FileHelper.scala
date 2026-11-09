package backuplab

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Try

object FileHelper {
  // исключения Java превращаются в типизированные ошибки на границе работы с файлами
  def attempt[A](operation: String, path: String)(action: => A): Either[AppError, A] = {
    Try(action).toEither.left.map(error =>
      AppError.IoFailure(operation, path, error.getClass.getSimpleName)
    )
  }

  def path(text: String): Either[AppError, Path] = {
    if (text.trim.isEmpty) Left(AppError.InvalidPath(text))
    else Try(Paths.get(text).toAbsolutePath.normalize()).toEither
      .left.map(_ => AppError.InvalidPath(text))
  }

  def deleteDirectory(directory: Path): Unit = {
    attempt("удаление рабочей папки", directory.toString) {
      if (Files.exists(directory)) {
        val paths = Files.walk(directory)
        try paths.iterator().asScala.toList.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists(_))
        finally paths.close()
      }
    }
    ()
  }

  // путь не должен выходить из корня или проходить через символическую ссылку
  def safePath(root: Path, relative: String): Either[AppError, Path] = {
    attempt("проверка пути", relative)(root.resolve(relative).normalize()).flatMap { target =>
      if (!target.startsWith(root)) Left(AppError.UnsafePath(relative))
      else {
        val parents = Iterator.iterate(Option(target))(_.flatMap(p => Option(p.getParent)))
          .takeWhile(_.nonEmpty).flatten.toList
        if (parents.exists(Files.isSymbolicLink(_))) Left(AppError.UnsafePath(relative))
        else Right(target)
      }
    }
  }
}
