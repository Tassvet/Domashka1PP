// здесь начинается выполнение программы: загружаем настройки, создаём журнал и запускаем ConsoleApp

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import java.nio.file.Paths

// Main существует в одном экземпляре; внутри находится метод запуска
object Main {
  // точка входа: ConfigService загружает настройки, затем создаём LogService и ConsoleApp
  // при неправильной конфигурации показываем ошибки и не запускаем меню
  // args — массив строк с аргументами запуска; Unit означает отсутствие полезного возвращаемого значения
  def main(args: Array[String]): Unit = {
    // получаем путь к config.properties; файл будет прочитан отдельно
    val configPath = Paths.get("config.properties")

    // match выбирает ветку по результату загрузки: Left с ошибками или Right с настройками
    ConfigService.loadOrCreate(configPath) match {
      // эта ветка обрабатывает ошибку; внутри Left находится сообщение или список ошибок
      case Left(errors) =>
        // создаём журнал со стандартным путём, потому что загруженные настройки неправильные
        val log = new LogService(Paths.get(ConfigService.defaultConfig.logFile))
        // записываем ошибку в журнал
        log.error("Конфигурация содержит ошибки")
        // println выводит сообщение и переводит курсор на следующую строку
        println("В конфигурации обнаружены ошибки:")
        // для каждой ошибки выводим строку; error => задаёт маленькую функцию
        errors.foreach(error => println(s"- $error"))

      // эта ветка выполняется если всё кул
      case Right(config) =>
        // создаём экземпляр LogService по пути из правильных настроек
        val log = new LogService(Paths.get(config.logFile))
        // записываем обычное информационное событие в журнал
        log.info("Конфигурация загружена и проверена")
        // создаём ConsoleApp и передаём путь настроек, сами настройки и журнал
        val application = new ConsoleApp(configPath, config, log)
        // запускаем меню и обработку команд в ConsoleApp
        application.run()
    }
  }
}

