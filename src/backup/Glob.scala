package backup

import java.nio.file.{FileSystems, PathMatcher, Paths}
import scala.util.control.NonFatal

object Glob:
  private def compile(pattern: String): Either[AppError, PathMatcher] =
    if pattern.isEmpty then return Left(AppError.InvalidPattern(pattern))
    if pattern.contains("\n") then return Left(AppError.InvalidPattern(pattern))
    if pattern.contains("\r") then return Left(AppError.InvalidPattern(pattern))
    // Java проверяет шаблоны вроде *.tmp и [abc]. Свой разбор шаблонов не нужен.
    try Right(FileSystems.getDefault.getPathMatcher("glob:" + pattern))
    catch case NonFatal(_) => Left(AppError.InvalidPattern(pattern))

  def isValidPattern(pattern: String): Boolean = compile(pattern).isRight

  def matches(pattern: String, name: String): Boolean =
    compile(pattern) match
      case Left(_) => false
      case Right(matcher) => matcher.matches(Paths.get(name))
