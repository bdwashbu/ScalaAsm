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

object HelloWorld extends AsmProgram {

  import com.scalaAsm.x86.Instructions.General._
  import com.scalaAsm.asm._
  
  sections += new DataSection(
    Variable("helloWorld", "Hello World!\r\n")
  ) {}

  sections += new CodeSection {
    procedure(name = "_main",
      asm"""push helloWorld
      call _printf
      pop ebx
      retn"""
    )
  }
}

object HelloWorldWin extends AsmProgram {
  import com.scalaAsm.x86.Instructions.General._

  import com.scalaAsm.asm._
  import universe._
  
  sections += new DataSection(
    Variable("helloWorld", "Hello World!\r\n\u0000")
  ) {}

  sections += new CodeSection {
    builder += Code(asm"""
      // DWORD  bytes;    
      mov     ebp, esp
      sub     esp, 4
  
      // hStdOut = GetstdHandle( STD_OUTPUT_HANDLE)
      push    -11
      call    GetStdHandle
      mov     ebx, eax    
  
      // WriteFile( hstdOut, message, length(message), &bytes, 0);
      push    0
      lea     eax, [ebp-4]
      push    eax
      push    12
      push    helloWorld
      push    ebx
      call    WriteFile
  
      // ExitProcess(0)
      push    0
      call    ExitProcess
      """
    )
  }
}


class HelloWorldTest extends FlatSpec with ShouldMatchers {
    
  val executableName = "test_HelloWorldTest.exe"
  
  "A c-library 32-bit Hello world" should "print 'Hello World'" in {
    getProgramOutput(HelloWorld, false) should equal("Hello World!")
  }
  
  "A windows-library 32-bit Hello world" should "print 'Hello World'" in {
    getProgramOutput(HelloWorldWin, false) should equal("Hello World!")
  }
}