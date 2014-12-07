name := "play_interface"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies += "asm" %% "asm" % "1.0"

libraryDependencies += "coff" %% "coff" % "1.0"

libraryDependencies += "linker" %% "linker" % "1.0"

libraryDependencies += "assembler" %% "assembler" % "1.0"

libraryDependencies += "portableexe" %% "portableexe" % "1.0"

libraryDependencies += "x86" %% "x86" % "1.0"

lazy val play_interface = project.in(file(".")).enablePlugins(PlayScala)

