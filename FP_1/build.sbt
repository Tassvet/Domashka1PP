ThisBuild / scalaVersion := "3.3.7"
ThisBuild / version := "1.0.0"
ThisBuild / organization := "backuplab"

lazy val root = (project in file("."))
  .settings(
    name := "BackupLab-FP1",
    Compile / run / fork := true,
    Compile / run / connectInput := true,
    Compile / mainClass := Some("backuplab.Main")
  )
