import play.Project._

name := "scala_x86"

version := "1.0"

playScalaSettings

lazy val scalax86 = project.in(file("."))
    .dependsOn(asm, x86, portableExe, example)
    
lazy val example = project.in(file("example"))
    .dependsOn(asm, x86, portableExe)
    
lazy val asm = project.in(file("asm"))
    .dependsOn(x86, utils)
    
lazy val x86 = project.in(file("x86"))
    .dependsOn(utils)
    
lazy val utils = project.in(file("utils"))

lazy val portableExe = project.in(file("portableExe"))
    .dependsOn(asm, x86)
