//import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.rshahin",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "spl",
	  libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4",
	  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
  )
