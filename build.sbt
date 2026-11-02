ThisBuild / scalaVersion := "3.7.4"
ThisBuild / organization := "backup"
ThisBuild / version      := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "backup",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.1.0",
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "backup",
    Compile / run / connectInput := true
  )
