ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "AlphaZeroImplementation",

  )
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
