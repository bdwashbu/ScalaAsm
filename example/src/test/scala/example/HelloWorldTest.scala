package example

import org.scalatest._
import java.io.DataOutputStream
import java.io.FileOutputStream
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream
import java.io.BufferedWriter
import java.io.File
import java.io.FileInputStream
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import com.scalaAsm.asm.AsmMacro
import com.scalaAsm.x86.Instructions.Catalog.Standard
import com.scalaAsm.x86.Instructions.Catalog
import com.scalaAsm.x86.InstructionResult

object HelloWorld extends AsmProgram[x86_32] {

  import com.scalaAsm.x86.Instructions.General._

  import com.scalaAsm.asm._
  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  import universe._
  
  implicit class OctalContext (val sc : StringContext) {

    def asm(): () => InstructionResult = macro AsmMacro.impl
  }
 
  
  
  sections += new DataSection(
    Variable("helloWorld", "Hello World!\r\n\u0000"),
    Variable("helloWorld2", "Hello Worldddd!\r\n\u0000")
  ) {}

  sections += new CodeSection {
    builder += Code(
      asm"push helloWorld",
      asm"call printf",
      asm"pop ebx",
      asm"retn"
    )
  }
}



class HelloWorldTest extends FlatSpec with ShouldMatchers {
    
  val executableName = "test_HelloWorldTest.exe"
  
  "A simple 32-bit Hello world" should "print 'Hello World'" in {
    val name = System.nanoTime
    val outputStream = new DataOutputStream(new FileOutputStream(executableName));
    val assembler = new Assembler {}
    val linker = new Linker {}  

    var beginTime = System.nanoTime()
    val helloWorld = assembler.assemble(HelloWorld).addIcon("scala.ico")

    val exe = linker.link(helloWorld, 0x3000, false, "kernel32.dll", "msvcrt.dll")

    outputStream.write(exe.get)
    println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
    outputStream.close

    val child = Runtime.getRuntime().exec(executableName);
    val in = new BufferedReader(
      new InputStreamReader(child.getInputStream()));

    val output = in.readLine()

    child.waitFor()

    new File(executableName).delete()

    output should equal("Hello World!")
  }
}