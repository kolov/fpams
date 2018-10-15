import Dependencies._

lazy val root = ( project in file(".") ).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "fpams",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies += scalaTest % Test,
    libraryDependencies ++= Seq(scalaz, zio)
  )


