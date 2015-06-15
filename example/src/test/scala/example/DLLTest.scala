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
import org.scalatest.concurrent.TimeLimitedTests

import com.scalaAsm.x86.InstructionResult
import com.scalaAsm.x86.Instructions._
import org.scalatest.time.SpanSugar._

object DLLBuilder extends AsmProgram {

  import com.scalaAsm.x86.Instructions.General._

  import com.scalaAsm.asm._
  import universe._

  sections += new CodeSection {
    
    procedure(name = "printHelloWorld",
      asm"mov eax, 4",
      asm"retn")
  }
}

object DLLRunner extends AsmProgram {

  import com.scalaAsm.x86.Instructions.General._

  import com.scalaAsm.asm._
  import universe._
  
  sections += new DataSection(
        Variable("test", "%d\n\u0000")) {}

  sections += new CodeSection {
    builder += Code(
      asm"""
        mov eax, 4
      //call printHelloWorld
      push eax
      push test
      call printf
      pop eax
      pop eax
      retn"""
    )
  }
}


class DLLTest extends FlatSpec with ShouldMatchers {
  //getDLLOutput(DLLBuilder, DLLRunner, false)
  "A c-library 32-bit Hello world" should "print 'Hello World'" in {
    getDLLOutput(DLLBuilder, DLLRunner, false) should equal("4")
  }
}