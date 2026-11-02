package backup

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

final class Logger(logFile: String):
  def info(message: String): Either[AppError, Unit] =
    Boundary.attempt("Запись журнала") {
      val path = Paths.get(logFile).toAbsolutePath
      Option(path.getParent).foreach(Files.createDirectories(_))
      val time = LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      Files.writeString(path, s"$time [INFO] $message\n", UTF_8,
        StandardOpenOption.CREATE, StandardOpenOption.APPEND)
      ()
    }
