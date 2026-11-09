package backuplab

import java.nio.file.Paths

object Main {
  def main(args: Array[String]): Unit = {
    val configPath = Paths.get("config.properties")
    ConfigService.loadOrCreate(configPath) match {
      case Left(error) => println(ErrorMessages.text(error))
      case Right(config) =>
        val log = new LogService(Paths.get(config.logFile))
        log.info("Конфигурация загружена")
        new ConsoleApp(configPath, config, log).run()
    }
  }
}
