package backuplab

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.{Base64, Properties}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

object StorageDataService {
  private val separator = "\u001f"

  def save(directory: Path, data: StorageData): Either[AppError, Unit] = {
    FileHelper.attempt("запись метаданных", directory.toString) {
      Files.createDirectories(directory.resolve("data"))
      val properties = new Properties()
      properties.setProperty("name", data.name)
      properties.setProperty("createdAt", data.createdAt)
      properties.setProperty("encrypted", data.encrypted.toString)
      properties.setProperty("hashAlgorithm", data.hashAlgorithm)
      properties.setProperty("nextSourceId", data.nextSourceId.toString)
      properties.setProperty("nextVersionId", data.nextVersionId.toString)
      Using.resource(Files.newBufferedWriter(directory.resolve("storage.properties"), StandardCharsets.UTF_8)) {
        writer => properties.store(writer, "BackupLab storage")
      }
      val sources = data.sources.sortBy(_.id).map { s =>
        List(s.id.toString, encode(s.path), s.includeSubdirectories.toString,
          encode(s.exclusions.mkString(separator)), s.enabled.toString,
          encode(s.lastSuccessfulBackup.getOrElse(""))).mkString("\t")
      }
      val versions = data.versions.sortBy(_.id).map { v =>
        val status = if (v.result == BackupStatus.Complete) "успешно" else "частично"
        List(v.id.toString, encode(v.createdAt), v.added.toString, v.modified.toString,
          v.deleted.toString, v.moved.toString, v.skipped.toString, v.writtenBytes.toString,
          encode(status), encode(v.comment.getOrElse(""))).mkString("\t")
      }
      val revisions = data.revisions.sortBy(r => (r.versionId, r.sourceId, r.relativePath)).map { r =>
        List(r.sourceId.toString, encode(r.relativePath), r.versionId.toString, r.changeType.toString,
          r.size.toString, r.modifiedTime.toString, r.hash, encode(r.dataFile.getOrElse(""))).mkString("\t")
      }
      // пустое поле используется только в старом формате на диске, в моделях хранится Option
      writeLines(directory.resolve("sources.tsv"), sources)
      writeLines(directory.resolve("versions.tsv"), versions)
      writeLines(directory.resolve("revisions.tsv"), revisions)
    }
  }

  def load(directory: Path): Either[AppError, StorageData] = {
    val path = directory.resolve("storage.properties")
    for {
      properties <- FileHelper.attempt("чтение метаданных", path.toString) {
        val result = new Properties()
        Using.resource(Files.newBufferedReader(path, StandardCharsets.UTF_8))(result.load)
        result
      }
      name <- required(properties, "name")
      created <- required(properties, "createdAt")
      encrypted <- required(properties, "encrypted").flatMap(boolean)
      hash <- required(properties, "hashAlgorithm")
      _ <- if (Set("SHA-256", "SHA-512").contains(hash)) Right(())
           else Left(AppError.CorruptMetadata("hashAlgorithm"))
      sourceId <- required(properties, "nextSourceId").flatMap(integer)
      versionId <- required(properties, "nextVersionId").flatMap(integer)
      sources <- parseLines(directory.resolve("sources.tsv"), 6)(readSource)
      versions <- parseLines(directory.resolve("versions.tsv"), 10)(readVersion)
      revisions <- parseLines(directory.resolve("revisions.tsv"), 8)(readRevision)
      _ <- if (sourceId > sources.map(_.id).maxOption.getOrElse(0) &&
               versionId > versions.map(_.id).maxOption.getOrElse(0)) Right(())
           else Left(AppError.CorruptMetadata("следующие номера"))
    } yield StorageData(name, created, encrypted, hash, sources, versions, revisions, sourceId, versionId)
  }

  private def readSource(f: Array[String]): Either[AppError, BackupSource] = {
    for {
      id <- integer(f(0))
      path <- decode(f(1))
      _ <- FileHelper.path(path).left.map(_ => AppError.CorruptMetadata("путь источника"))
      include <- boolean(f(2))
      exclusions <- decode(f(3))
      enabled <- boolean(f(4))
      last <- decode(f(5))
    } yield BackupSource(id, path, include,
      if (exclusions.isEmpty) Nil else exclusions.split(separator, -1).toList,
      enabled, optional(last))
  }

  private def readVersion(f: Array[String]): Either[AppError, BackupVersion] = {
    for {
      id <- integer(f(0))
      created <- decode(f(1))
      added <- integer(f(2))
      modified <- integer(f(3))
      deleted <- integer(f(4))
      moved <- integer(f(5))
      skipped <- integer(f(6))
      bytes <- long(f(7))
      statusText <- decode(f(8))
      status <- statusText match {
        case "успешно" => Right(BackupStatus.Complete)
        case "частично" => Right(BackupStatus.Partial)
        case _ => Left(AppError.CorruptMetadata("результат версии"))
      }
      comment <- decode(f(9))
    } yield BackupVersion(id, created, added, modified, deleted, moved, skipped, bytes, status, optional(comment))
  }

  private def readRevision(f: Array[String]): Either[AppError, FileRevision] = {
    for {
      sourceId <- integer(f(0))
      relative <- decode(f(1))
      version <- integer(f(2))
      change <- ChangeType.values.find(_.toString == f(3)).toRight(AppError.CorruptMetadata("тип изменения"))
      size <- long(f(4))
      modified <- long(f(5))
      data <- decode(f(7))
      _ <- if (relative.nonEmpty && (change == ChangeType.DELETED || data.nonEmpty)) Right(())
           else Left(AppError.CorruptMetadata("запись файла"))
    } yield FileRevision(sourceId, relative, version, change, size, modified, f(6), optional(data))
  }

  private def parseLines[A](path: Path, count: Int)(read: Array[String] => Either[AppError, A])
      : Either[AppError, List[A]] = {
    FileHelper.attempt("чтение таблицы", path.toString) {
      Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toList.filter(_.nonEmpty)
    }.flatMap { lines =>
      // flatMap прекращает разбор, если одна из предыдущих строк повреждена
      lines.foldLeft[Either[AppError, List[A]]](Right(Nil)) { (result, line) =>
        val fields = line.split("\t", -1)
        for {
          previous <- result
          item <- if (fields.length == count) read(fields)
                  else Left(AppError.CorruptMetadata(path.getFileName.toString))
        } yield item :: previous
      }.map(_.reverse)
    }
  }

  private def required(p: Properties, name: String): Either[AppError, String] = {
    Option(p.getProperty(name)).filter(_.nonEmpty).toRight(AppError.CorruptMetadata(name))
  }
  private def integer(text: String): Either[AppError, Int] =
    text.toIntOption.filter(_ >= 0).toRight(AppError.CorruptMetadata("целое число"))
  private def long(text: String): Either[AppError, Long] =
    text.toLongOption.filter(_ >= 0).toRight(AppError.CorruptMetadata("размер или время"))
  private def boolean(text: String): Either[AppError, Boolean] = text match {
    case "true" => Right(true)
    case "false" => Right(false)
    case _ => Left(AppError.CorruptMetadata("логическое значение"))
  }
  private def optional(text: String): Option[String] = Option(text).filter(_.nonEmpty)
  private def writeLines(path: Path, lines: List[String]): Unit = {
    Files.write(path, lines.asJava, StandardCharsets.UTF_8)
    ()
  }
  private def encode(text: String): String = Base64.getEncoder.encodeToString(text.getBytes(StandardCharsets.UTF_8))
  private def decode(text: String): Either[AppError, String] = {
    Try(new String(Base64.getDecoder.decode(text), StandardCharsets.UTF_8)).toEither
      .left.map(_ => AppError.CorruptMetadata("Base64 в таблице"))
  }
}
