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

import com.scalaAsm.x86.InstructionResult
import com.scalaAsm.x86.Operands._

object HelloWorld extends AsmProgram {

  import com.scalaAsm.x86.Instructions.General._

  import com.scalaAsm.asm._
  import universe._
  
  sections += new DataSection(
    Variable("helloWorld", "Hello World!\r\n\u0000"),
    Variable("helloWorld2", "Hello Worldddd!\r\n\u0000")
  ) {}

  sections += new CodeSection {
    builder += Code(
      asm"""push helloWorld
      call printf
      pop ebx
      retn"""
    )
  }
}


class HelloWorldTest extends FlatSpec with ShouldMatchers {
    
  val executableName = "test_HelloWorldTest.exe"
  
  "A simple 32-bit Hello world" should "print 'Hello World'" in {
    getProgramOutput(HelloWorld, false) should equal("Hello World!")
  }
}