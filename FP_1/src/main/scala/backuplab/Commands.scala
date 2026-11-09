package backuplab

// строка разбирается один раз, дальше в меню используется Command
enum Command {
  case Create, Open, Info, Sources, Scan, Backup, Versions, Restore, Verify, Reload, Help, Exit, Empty
}

object Command {
  def parse(input: String): Either[AppError, Command] = input.trim.toLowerCase match {
    case "create" => Right(Command.Create)
    case "open" => Right(Command.Open)
    case "info" => Right(Command.Info)
    case "sources" => Right(Command.Sources)
    case "scan" => Right(Command.Scan)
    case "backup" => Right(Command.Backup)
    case "versions" => Right(Command.Versions)
    case "restore" => Right(Command.Restore)
    case "verify" => Right(Command.Verify)
    case "reload" => Right(Command.Reload)
    case "help" => Right(Command.Help)
    case "exit" => Right(Command.Exit)
    case "" => Right(Command.Empty)
    case _ => Left(AppError.UnknownCommand(input))
  }
}

enum SourceCommand {
  case Add, Edit, Remove, Toggle, Back, Empty
}

object SourceCommand {
  def parse(input: String): Either[AppError, SourceCommand] = input.trim.toLowerCase match {
    case "add" => Right(SourceCommand.Add)
    case "edit" => Right(SourceCommand.Edit)
    case "remove" => Right(SourceCommand.Remove)
    case "toggle" => Right(SourceCommand.Toggle)
    case "back" => Right(SourceCommand.Back)
    case "" => Right(SourceCommand.Empty)
    case _ => Left(AppError.UnknownSourceCommand(input))
  }
}
