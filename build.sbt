ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "testing-homework",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.14" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % "test"
    ),
    scalacOptions += "-Xfatal-warnings"
  )
