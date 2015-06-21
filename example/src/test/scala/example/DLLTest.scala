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
    
    procedure(name = "addMe",asm"""
      push ebp
      mov ebp, esp
      mov eax, [ebp + 8]
      mov edx, [ebp + 12]
      add eax, edx
      pop ebp
      retn
      """)
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
      push 1
      push 3
      call addMe
      add esp, 8
      push eax
      push test
      call printf
      pop eax
      pop eax
      retn"""
    )
  }
}

object DLLBuilder2 extends AsmProgram {

  import com.scalaAsm.x86.Instructions.General._

  import com.scalaAsm.asm._
  import universe._

  sections += new DataSection(
        Variable("helloWorld", "Hello World!\r\n\u0000")) {}
  
  sections += new CodeSection {
    
    procedure(name = "printHello",asm"""
      push test
      call printf
      pop eax
      retn
      """)
  }
}

object DLLRunner2 extends AsmProgram {

  import com.scalaAsm.x86.Instructions.General._

  import com.scalaAsm.asm._
  import universe._

  sections += new CodeSection {
    
    builder += Code(
      asm"""
      call printHello
      retn"""
    )
  }
}


class DLLTest extends FlatSpec with ShouldMatchers {
  //getDLLOutput(DLLBuilder, DLLRunner, false)
  "A simple function found in a dll" should "be found and print '4'" in {   
    getDLLOutput(DLLBuilder, DLLRunner, false) should equal("4")
  }
  
  "A dll with imports" should "work fine" in {
    //getDLLOutput(DLLBuilder2, DLLRunner2, Seq("kernel32.dll", "msvcrt.dll"), Nil, false) should equal("Hello World!")
  }
}