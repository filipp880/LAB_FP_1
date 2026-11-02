package backup

import java.nio.file.{FileSystems, PathMatcher, Paths}

object Glob:
  private def compile(pattern: String): Either[AppError, PathMatcher] =
    if pattern.isEmpty || pattern.exists(c => c == '\n' || c == '\r') then
      Left(AppError.InvalidPattern(pattern))
    else Boundary.attempt("Разбор glob-шаблона") {
      FileSystems.getDefault.getPathMatcher("glob:" + pattern)
    }.left.map(_ => AppError.InvalidPattern(pattern))

  def isValidPattern(pattern: String): Boolean = compile(pattern).isRight

  def matches(pattern: String, name: String): Boolean =
    compile(pattern).toOption.exists(_.matches(Paths.get(name)))
