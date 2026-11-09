package backuplab

import scala.io.StdIn

object InputHelper {
  def read(prompt: String): Either[AppError, String] = {
    print(prompt)
    FileHelper.attempt("чтение ввода", "консоль")(Option(StdIn.readLine()))
      .flatMap(_.map(_.trim).toRight(AppError.InputClosed))
  }

  def readWithDefault(prompt: String, default: String): Either[AppError, String] = {
    read(s"$prompt [$default]: ").map(value => if (value.isEmpty) default else value)
  }

  def readYesNo(prompt: String, default: Boolean): Either[AppError, Boolean] = {
    val defaultText = if (default) "да" else "нет"
    var answer: Option[Either[AppError, Boolean]] = None
    while (answer.isEmpty) {
      read(s"$prompt [$defaultText]: ") match {
        case Left(error) => answer = Some(Left(error))
        case Right(value) => value.toLowerCase match {
          case "" => answer = Some(Right(default))
          case "да" | "д" | "yes" | "y" => answer = Some(Right(true))
          case "нет" | "н" | "no" | "n" => answer = Some(Right(false))
          case _ => println("Введите да или нет.")
        }
      }
    }
    answer.toRight(AppError.InputClosed).flatMap(identity)
  }

  def readInt(prompt: String, minimum: Int, maximum: Int): Either[AppError, Int] = {
    var answer: Option[Either[AppError, Int]] = None
    while (answer.isEmpty) {
      read(prompt) match {
        case Left(error) => answer = Some(Left(error))
        case Right(text) => text.toIntOption.filter(n => n >= minimum && n <= maximum) match {
          case Some(number) => answer = Some(Right(number))
          case None => println(s"Введите число от $minimum до $maximum.")
        }
      }
    }
    answer.toRight(AppError.InputClosed).flatMap(identity)
  }

  def formatBytes(bytes: Long): String = {
    if (bytes < 1024) s"$bytes байт"
    else if (bytes < 1024L * 1024) f"${bytes / 1024.0}%.1f КБ"
    else if (bytes < 1024L * 1024 * 1024) f"${bytes / (1024.0 * 1024)}%.1f МБ"
    else f"${bytes / (1024.0 * 1024 * 1024)}%.1f ГБ"
  }
}
