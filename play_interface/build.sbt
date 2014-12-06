name := "play_interface"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies += "com.scalaAsm" %% "scala_x86" % "0.1-SNAPSHOT"

lazy val play_interface = project.in(file(".")).enablePlugins(PlayScala)

