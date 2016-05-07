//import play.Project._

name := "scala_x86"

version := "0.1-SNAPSHOT"

organization := "com.scalaAsm"

scalaVersion := "2.11.6"

//playScalaSettings

lazy val scalax86 = project.in(file(".")).aggregate(x86, coff, asm, assembler, portableExe, linker, example, testing)
    
lazy val example = project.in(file("example"))
    .dependsOn(asm, assembler, linker)

lazy val testing = project.in(file("testing"))
    .dependsOn(asm, assembler, linker, x86, portableExe, coff)
    
lazy val coff = project.in(file("coff"))

lazy val x86 = project.in(file("x86"))

lazy val asm = project.in(file("asm"))
    .dependsOn(asmMacro)

lazy val asmMacro = project.in(file("asmMacro"))
    .dependsOn(x86)

lazy val portableExe = project.in(file("portableExe"))
    .dependsOn(x86, coff)

lazy val linker = project.in(file("linker"))
    .dependsOn(asm, portableExe)

lazy val assembler = project.in(file("assembler"))
    .dependsOn(asm, coff)

libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"