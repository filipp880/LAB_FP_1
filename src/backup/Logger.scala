package backup

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.control.NonFatal

final class Logger(logFile: String):
  def info(message: String): Either[AppError, Unit] =
    try
      val path = Paths.get(logFile).toAbsolutePath
      val time = LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      Files.writeString(path, s"$time [INFO] $message\n", UTF_8,
        StandardOpenOption.CREATE, StandardOpenOption.APPEND)
      Right(())
    catch case NonFatal(_) => Left(AppError.WriteFailed(logFile))
