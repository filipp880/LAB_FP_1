ThisBuild / scalaVersion := "3.7.4"
ThisBuild / organization := "backup"
ThisBuild / version      := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "backup",
    libraryDependencies += "com.lihaoyi" %% "ujson" % "4.1.0",
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "backup",
    Test / unmanagedSourceDirectories += baseDirectory.value / "tests",
    Compile / run / connectInput := true
  )
