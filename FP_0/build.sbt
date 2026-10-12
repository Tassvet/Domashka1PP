ThisBuild / scalaVersion := "3.9.0"
ThisBuild / version := "1.0.0"
ThisBuild / organization := "backuplab"

lazy val root = (project in file("."))
  .settings(
    name := "BackupLab",
    Compile / run / fork := true
  )
