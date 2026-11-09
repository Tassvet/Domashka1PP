package backuplab

import java.nio.file.{Files, Path, StandardCopyOption}

// без тестовых библиотек: запускается командой Test / runMain backuplab.BasicScenarios
object BasicScenarios {
  private var checks = 0

  private def check(condition: Boolean, name: String): Unit = {
    assert(condition, name)
    checks += 1
    println(s"OK: $name")
  }

  private def right[A](result: Either[AppError, A]): A = result match {
    case Right(value) => value
    case Left(error) => throw new AssertionError(ErrorMessages.text(error))
  }

  def main(args: Array[String]): Unit = {
    val root = Files.createTempDirectory("backuplab-tests-")
    try {
      basic(root)
      partialBackup(root)
      encryption(root)
      failures(root)
      compatibility(args)
      println(s"Проверок пройдено: $checks")
    } finally FileHelper.deleteDirectory(root)
  }

  private def basic(root: Path): Unit = {
    val source = Files.createDirectories(root.resolve("source"))
    val hello = source.resolve("hello.txt")
    val notes = source.resolve("notes.txt")
    Files.writeString(hello, "Привет, версия 1")
    Files.writeString(notes, "Тестовая заметка")
    Files.writeString(source.resolve("skip.tmp"), "не копировать")
    val config = ConfigService.defaultConfig
    val container = root.resolve("backup.store")
    val session = right(ContainerService.create(container, "Тест", false, None, "SHA-256"))
    try {
      right(SourceService.addSource(session, source.toString, true, config.defaultExclusions))
      val first = ScanService.scan(session)
      check(first.addedCount == 2 && first.problems.isEmpty, "первая проверка и исключение *.tmp")
      val backup1 = right(BackupService.createBackup(session, first, Some("Первая копия"), config))
      check(backup1.versionId == 1 && backup1.added == 2, "первая версия")
      check(VerifyService.verify(session).problems.isEmpty, "целостность после первой копии")
      check(ScanService.scan(session).changes.isEmpty, "после копирования изменений нет")
      check(BackupService.createBackup(session, ScanService.scan(session), None, config) == Left(AppError.NoChanges),
        "пустая версия не создаётся")
      val plan1 = right(RestoreService.plan(session, 1, root.resolve("restore_v1")))
      val restored1 = right(RestoreService.restore(session, plan1, Set.empty))
      check(restored1.restored == 2 && restored1.failed == 0, "восстановление первой версии")
      check(Files.readString(root.resolve("restore_v1/1_source/hello.txt")) == "Привет, версия 1", "содержимое первой версии")

      Files.writeString(hello, "Привет, версия 2")
      Files.delete(notes)
      Files.writeString(source.resolve("new.txt"), "Новый файл")
      val second = ScanService.scan(session)
      check(second.addedCount == 1 && second.modifiedCount == 1 && second.deletedCount == 1, "три вида изменений")
      val backup2 = right(BackupService.createBackup(session, second, None, config))
      check(backup2.versionId == 2 && backup2.problems.isEmpty, "вторая версия")
      val plan2 = right(RestoreService.plan(session, 2, root.resolve("restore_v2")))
      val restored2 = right(RestoreService.restore(session, plan2, Set.empty))
      check(restored2.restored == 2 && !Files.exists(root.resolve("restore_v2/1_source/notes.txt")), "удалённый файл не восстанавливается")
      val oldPlan = right(RestoreService.plan(session, 1, root.resolve("restore_old")))
      right(RestoreService.restore(session, oldPlan, Set.empty))
      check(Files.readString(root.resolve("restore_old/1_source/hello.txt")) == "Привет, версия 1" &&
        Files.exists(root.resolve("restore_old/1_source/notes.txt")), "старая версия доступна после второй копии")
      check(right(RestoreService.restore(session, plan2, Set.empty)).skipped == 2, "пропуск существующих файлов")
      val restoredHello = root.resolve("restore_v2/1_source/hello.txt")
      Files.writeString(restoredHello, "локальные изменения")
      val overwritten = right(RestoreService.restore(session, plan2, Set(restoredHello)))
      check(overwritten.restored == 1 && overwritten.skipped == 1 && Files.readString(restoredHello) == "Привет, версия 2",
        "выборочная перезапись по готовым ответам интерфейса")
      Files.move(source.resolve("new.txt"), source.resolve("renamed.txt"))
      val moved = ScanService.scan(session)
      check(moved.movedCount == 1 && moved.addedCount == 0 && moved.deletedCount == 0, "переименование")
      val movedBackup = right(BackupService.createBackup(session, moved, None, config))
      check(movedBackup.writtenBytes == 0, "перемещение повторно использует блок")
      right(SourceService.toggleSource(session, 1))
      Files.writeString(hello, "не проверяется")
      check(ScanService.scan(session).changes.isEmpty, "отключённый источник пропускается")
      right(SourceService.toggleSource(session, 1))
      Files.writeString(hello, "Привет, версия 2")
      right(SourceService.editSource(session, 1, source.toString, true, List("*.txt", "*.tmp")))
      check(ScanService.scan(session).changes.isEmpty, "новые исключения не создают ложных удалений")
      right(SourceService.editSource(session, 1, source.toString, true, config.defaultExclusions))
      val movedSource = root.resolve("source-offline")
      Files.move(source, movedSource)
      val unavailable = ScanService.scan(session)
      check(unavailable.changes.isEmpty && unavailable.problems.exists {
        case AppError.SourceUnavailable(_) => true
        case _ => false
      }, "недоступный источник не считается удалением")
      Files.move(movedSource, source)
      val reopened = right(ContainerService.open(container, None))
      try check(reopened.data.versions.size == 3 && VerifyService.verify(reopened).problems.isEmpty, "повторное открытие контейнера")
      finally ContainerService.close(reopened)
      check(RestoreService.plan(session, 999, root.resolve("bad")) == Left(AppError.VersionNotFound(999)), "ошибка номера версии")
      val unsafe = session.data.revisions.find(_.dataFile.nonEmpty).map(_.copy(relativePath = "../../escape.txt", versionId = 1))
      val old = session.data
      session.data = old.copy(revisions = unsafe.toList)
      check(RestoreService.plan(session, 1, root.resolve("unsafe")).isLeft, "небезопасный путь отклоняется")
      session.data = old
      val block = session.data.revisions.flatMap(_.dataFile).head
      Files.writeString(session.workDirectory.resolve(block), "повреждено")
      check(VerifyService.verify(session).problems.exists {
        case AppError.HashMismatch(_) => true
        case _ => false
      }, "повреждение блока обнаруживается")
    } finally ContainerService.close(session)
  }

  private def partialBackup(root: Path): Unit = {
    val source = Files.createDirectories(root.resolve("partial-source"))
    Files.writeString(source.resolve("a.txt"), "первая версия a")
    Files.writeString(source.resolve("b.txt"), "первая версия b")
    val config = ConfigService.defaultConfig
    val session = right(ContainerService.create(root.resolve("partial.store"), "Частичная копия", false, None, "SHA-256"))
    try {
      right(SourceService.addSource(session, source.toString, true, Nil))
      right(BackupService.createBackup(session, ScanService.scan(session), None, config))
      Files.writeString(source.resolve("a.txt"), "вторая версия a")
      Files.writeString(source.resolve("b.txt"), "вторая версия b")
      val scanned = ScanService.scan(session)
      Files.writeString(source.resolve("a.txt"), "изменился после scan")
      val partial = right(BackupService.createBackup(session, scanned, None, config))
      check(partial.modified == 1 && partial.skipped == 1, "частичный успех при изменении файла после scan")
      check(session.data.versions.last.result == BackupStatus.Partial, "статус частичной версии")
      val destination = root.resolve("partial-restored")
      val plan = right(RestoreService.plan(session, 2, destination))
      right(RestoreService.restore(session, plan, Set.empty))
      check(Files.readString(destination.resolve("1_partial-source/a.txt")) == "первая версия a" &&
        Files.readString(destination.resolve("1_partial-source/b.txt")) == "вторая версия b",
        "частичная копия сохраняет старые данные пропущенного файла")
      val next = ScanService.scan(session)
      Files.delete(source.resolve("a.txt"))
      check(BackupService.createBackup(session, next, None, config).isLeft && session.data.versions.size == 2,
        "если не удалось сохранить ничего, версия не создаётся")
    } finally ContainerService.close(session)
  }

  private def encryption(root: Path): Unit = {
    val key = Array.fill[Byte](32)(-5)
    check(java.util.Arrays.equals(right(CryptoService.textToKey(CryptoService.keyToText(key))), key),
      "ключ с дефисами Base64URL читается обратно")
    check(java.util.Arrays.equals(right(CryptoService.textToKey(java.util.Base64.getUrlEncoder.withoutPadding().encodeToString(key))), key),
      "чтение ключа без разделителей")
    val source = root.resolve("secret.txt")
    Files.writeString(source, "секретное содержимое")
    val container = root.resolve("encrypted.store")
    val session = right(ContainerService.create(container, "Шифрованный тест", true, Some(key), "SHA-256"))
    try {
      right(SourceService.addSource(session, source.toString, false, Nil))
      right(BackupService.createBackup(session, ScanService.scan(session), None, ConfigService.defaultConfig))
    } finally ContainerService.close(session)
    val reopened = right(ContainerService.open(container, Some(key)))
    try {
      check(VerifyService.verify(reopened).problems.isEmpty, "открытие шифрованного контейнера")
      val plan = right(RestoreService.plan(reopened, 1, root.resolve("secret-restored")))
      check(right(RestoreService.restore(reopened, plan, Set.empty)).restored == 1, "восстановление шифрованного файла")
    } finally ContainerService.close(reopened)
    check(ContainerService.open(container, Some(Array.fill[Byte](32)(1))) == Left(AppError.CannotDecrypt), "неверный ключ")
    check(ContainerService.open(container, None) == Left(AppError.KeyRequired), "отсутствующий ключ")
    val tampered = root.resolve("tampered.store")
    val bytes = Files.readAllBytes(container)
    bytes(bytes.length - 1) = (bytes.last ^ 1).toByte
    Files.write(tampered, bytes)
    check(ContainerService.open(tampered, Some(key)) == Left(AppError.CannotDecrypt), "проверка подлинности шифрованных данных")
  }

  private def failures(root: Path): Unit = {
    check(Command.parse(" BACKUP ") == Right(Command.Backup), "команда разбирается в ADT")
    check(Command.parse("wrong") == Left(AppError.UnknownCommand("wrong")), "неизвестная команда")
    check(SourceCommand.parse("edit") == Right(SourceCommand.Edit), "команды источников тоже ADT")
    check(CryptoService.textToKey("bad") == Left(AppError.InvalidKey), "ошибка формата ключа")
    check(ContainerService.open(root.resolve("missing.store"), None).isLeft, "отсутствующий контейнер")
    val text = root.resolve("ordinary.txt")
    Files.writeString(text, "не контейнер")
    check(ContainerService.open(text, None) == Left(AppError.InvalidContainer), "обычный файл не открывается как контейнер")
    val config = root.resolve("config.properties")
    val loaded = right(ConfigService.loadOrCreate(config))
    Files.writeString(config, Files.readString(config).replace("maximumVersionsPerFile=10", "maximumVersionsPerFile=0"))
    check(ConfigService.load(config).isLeft, "ошибка конфигурации")
    check(loaded.maximumVersionsPerFile == 10, "предыдущее значение конфигурации не меняется")
  }

  private def compatibility(args: Array[String]): Unit = {
    args.foreach { path =>
      val session = right(ContainerService.open(Path.of(path), None))
      try {
        check(session.data.versions.nonEmpty && VerifyService.verify(session).problems.isEmpty, "чтение контейнера, созданного ФП0")
        val destination = Files.createTempDirectory("fp0-restored-")
        try {
          val plan = right(RestoreService.plan(session, 1, destination))
          check(right(RestoreService.restore(session, plan, Set.empty)).restored == 2, "восстановление данных ФП0")
        } finally FileHelper.deleteDirectory(destination)
      } finally ContainerService.close(session)
    }
  }
}
