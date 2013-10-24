import sbt._
import Keys._

object HelloBuild extends Build {
    lazy val root = Project(id = "root",
                            base = file(".")) aggregate(asm, x86, portableExe, example) dependsOn(asm, x86, portableExe, example)
                            
    lazy val example = Project(id = "example",
                            base = file("example")) dependsOn(asm, x86, portableExe)

    lazy val asm = Project(id = "asm",
                           base = file("asm")) dependsOn(x86, utils)

    lazy val x86 = Project(id = "x86",
                           base = file("x86")) dependsOn(utils)
                           
    lazy val utils = Project(id = "utils",
                           base = file("utils"))
                           
    lazy val portableExe = Project(id = "portableExe",
                           base = file("portableExe")) dependsOn(asm, x86)
}