// здесь собраны ввод строк, ответов да/нет, чисел и показ размеров
// ConsoleApp вызывает эти методы, чтобы не повторять проверку ввода в каждом меню

// этот файл относится к общему пакету backuplab
package backuplab

// подключаем готовые средства, которые используются ниже
import scala.io.StdIn

// отдельная ошибка, означающая, что ввод закрылся; она нужна для завершения программы
class InputEndedException extends RuntimeException

object InputHelper {
  def read(prompt: String): String = {
    // print выводит приглашение без перехода на следующую строку
    print(prompt)
    // ждём ввод строки; null означает, что поток ввода закрыт
    val value = StdIn.readLine()
    // при закрытом вводе сообщаем об этом через отдельное исключение
    if (value == null) throw new InputEndedException
    // убираем пробелы в начале и конце введённой строки
    value.trim
  }

  def readWithDefault(prompt: String, defaultValue: String): String = {
    // s позволяет подставить prompt и defaultValue в текст приглашения
    val value = read(s"$prompt [$defaultValue]: ")
    // если пользователь просто нажал Enter, используем значение по умолчанию
    if (value.isEmpty) defaultValue else value
  }

  def readYesNo(prompt: String, defaultValue: Boolean): Boolean = {
    // готовим подсказку с ответом, который выберется при Enter
    val defaultText = if (defaultValue) "да" else "нет"

    // повторяем ввод до корректного ответа; успешная ветка выйдет через return
    while (true) {
      // читаем ответ; нижний регистр позволяет одинаково обработать Да и да
      val value = read(s"$prompt [$defaultText]: ").toLowerCase

      // пустой ответ означает нажатие Enter без текста
      if (value.isEmpty) {
        // выходим из метода с ответом по умолчанию
        return defaultValue
      }

      // || означает или: принимаем любой перечисленный положительный ответ
      if (value == "да" || value == "д" || value == "yes" || value == "y") {
        // возвращаем положительный ответ
        return true
      }

      // принимаем любой из перечисленных отрицательных ответов
      if (value == "нет" || value == "н" || value == "no" || value == "n") {
        // возвращаем отрицательный ответ
        return false
      }

      // выводим смску или результат юзеру
      println("Введите 'да' или 'нет'.")
    }

    // запасное выражение; при правильном ответе метод выходит раньше
    defaultValue
  }

  def readInt(prompt: String, minimum: Int, maximum: Int): Int = {
    // повторяем ввод до корректного ответа; успешная ветка выйдет через return
    while (true) {
      // получаем очередную строку от пользователя
      val value = read(prompt)

      // пробуем получить число: Some содержит число, None означает неправильный ввод
      value.toIntOption match {
        // значение есть; извлекаем его из Some и используем в этой ветке
        // принимаем число только в разрешённых границах
        case Some(number) if number >= minimum && number <= maximum =>
          // правильное число возвращаем вызывающему методу
          return number
        case _ =>
          // выводим смску или результат юзеру
          println(s"Введите число от $minimum до $maximum.")
      }
    }

    // запасное выражение после цикла; при правильном вводе до него не доходим
    minimum
  }

  def formatBytes(bytes: Long): String = {
    // маленький размер показываем в байтах
    if (bytes < 1024) {
      s"$bytes байт"
    // размер меньше мегабайта показываем в килобайтах
    } else if (bytes < 1024L * 1024) {
      f"${bytes / 1024.0}%.1f КБ"
    // размер меньше гигабайта показываем в мегабайтах
    } else if (bytes < 1024L * 1024 * 1024) {
      f"${bytes / (1024.0 * 1024)}%.1f МБ"
    } else {
      f"${bytes / (1024.0 * 1024 * 1024)}%.1f ГБ"
    }
  }
}

