package backup

import java.nio.file.Files

@main def backupApp(): Unit =
  val configPath = Config.defaultPath
  val initialized =
    if Files.notExists(configPath) then Config.save(configPath, Config.Default)
    else Right(())
  initialized.flatMap(_ => Config.load(configPath)) match
    case Left(error) => Console.err.println(error.message)
    case Right(config) => new Menu(config).run()
