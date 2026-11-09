package backuplab

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Properties
import scala.jdk.CollectionConverters.*
import scala.util.Using

object ConfigService {
  val defaultConfig = AppConfig(
    "backup.log", "keys", "backup.store", true, true, true, false,
    10, "SHA-256", List("*.tmp", "*.log", "cache", "node_modules")
  )

  def loadOrCreate(path: Path): Either[AppError, AppConfig] = {
    val prepared = FileHelper.attempt("подготовка конфигурации", path.toString) {
      if (!Files.exists(path)) {
        val text = """logFile=backup.log
          |keyDirectory=keys
          |defaultContainerName=backup.store
          |checkOnStorageOpen=true
          |confirmBeforeBackup=true
          |confirmBeforeRestoreOverwrite=true
          |overwriteExistingWithoutConfirmation=false
          |maximumVersionsPerFile=10
          |hashAlgorithm=SHA-256
          |defaultExclusions=*.tmp,*.log,cache,node_modules
          |""".stripMargin
        Option(path.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
        Files.writeString(path, text, StandardCharsets.UTF_8)
      }
    }
    prepared.flatMap(_ => load(path))
  }

  def load(path: Path): Either[AppError, AppConfig] = {
    readProperties(path).flatMap { properties =>
      val checked = for {
        logFile <- required(properties, "logFile")
        _ <- FileHelper.path(logFile)
        keyDirectory <- required(properties, "keyDirectory")
        name <- required(properties, "defaultContainerName")
        check <- boolean(properties, "checkOnStorageOpen")
        confirmBackup <- boolean(properties, "confirmBeforeBackup")
        confirmRestore <- boolean(properties, "confirmBeforeRestoreOverwrite")
        overwrite <- boolean(properties, "overwriteExistingWithoutConfirmation")
        limitText <- required(properties, "maximumVersionsPerFile")
        limit <- limitText.toIntOption.filter(_ >= 1)
          .toRight(AppError.InvalidSetting("maximumVersionsPerFile", limitText))
        hash <- required(properties, "hashAlgorithm").map(_.toUpperCase)
        _ <- if (Set("SHA-256", "SHA-512").contains(hash)) Right(())
             else Left(AppError.InvalidSetting("hashAlgorithm", hash))
        rulesText <- Option(properties.getProperty("defaultExclusions"))
          .toRight(AppError.MissingSetting("defaultExclusions"))
        rules = rulesText.split(",").map(_.trim).filter(_.nonEmpty).toList
        _ <- SourceService.validateExclusions(rules)
        directory <- FileHelper.path(keyDirectory)
        _ <- FileHelper.attempt("создание каталога ключей", keyDirectory) {
          Files.createDirectories(directory)
        }
        _ <- if (Files.isWritable(directory)) Right(())
             else Left(AppError.DestinationUnavailable(keyDirectory))
      } yield AppConfig(logFile, keyDirectory, name, check, confirmBackup, confirmRestore,
        overwrite, limit, hash, rules)
      checked.left.map {
        case error: AppError.IoFailure => error
        case error => AppError.InvalidConfig(List(error))
      }
    }
  }

  private def readProperties(path: Path): Either[AppError, Properties] = {
    FileHelper.attempt("чтение конфигурации", path.toString) {
      val lines = Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toList
      val malformed = lines.zipWithIndex.collect {
        case (line, index) if line.trim.nonEmpty && !line.trim.startsWith("#") && !line.contains("=") =>
          AppError.InvalidSetting(s"строка ${index + 1}", line)
      }
      val properties = new Properties()
      Using.resource(Files.newBufferedReader(path, StandardCharsets.UTF_8))(properties.load)
      (properties, malformed)
    }.flatMap { case (properties, errors) =>
      if (errors.isEmpty) Right(properties) else Left(AppError.InvalidConfig(errors))
    }
  }

  private def required(properties: Properties, name: String): Either[AppError, String] = {
    Option(properties.getProperty(name)).map(_.trim).filter(_.nonEmpty)
      .toRight(AppError.MissingSetting(name))
  }

  private def boolean(properties: Properties, name: String): Either[AppError, Boolean] = {
    required(properties, name).flatMap { value =>
      value.toLowerCase match {
        case "true" => Right(true)
        case "false" => Right(false)
        case _ => Left(AppError.InvalidSetting(name, value))
      }
    }
  }
}
