//import play.Project._

name := "scala_x86"

version := "1.0"

scalaVersion := "2.11.0"

//playScalaSettings

lazy val scalax86 = project.in(file("."))
    .dependsOn(example)
    
lazy val example = project.in(file("example"))
    .dependsOn(asm, linker, assembler)
    
lazy val asm = project.in(file("asm"))
    .dependsOn(x86)
    
lazy val x86 = project.in(file("x86"))

lazy val portableExe = project.in(file("portableExe"))
    .dependsOn(asm, x86)

lazy val coff = project.in(file("coff"))

lazy val linker = project.in(file("linker"))
    .dependsOn(portableExe, coff)

lazy val assembler = project.in(file("assembler"))
    .dependsOn(asm, linker, x86)