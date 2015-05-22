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
import com.scalaAsm.asm.x86_64
import com.scalaAsm.asm._

object HelloWorld64 extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.General._
  import com.scalaAsm.x86.Operands._
  
  sections += new DataSection (
    Variable("helloWorld", "Hello World!\r\n\u0000")
  ) {}

  sections += new CodeSection {

    procedure(name = "start",
      asm"push rsp",
      asm"push [rsp]",
      asm"and spl, 0xF0",
      asm"mov rdx, dword 0xE",
      List(LEA(rcx, addr("helloWorld"))),
      asm"sub rsp, 0x20",
      asm"invoke printf", // needs work
      asm"lea rsp, [rsp + 0x28]",
      asm"pop rsp",
      asm"xor rax, rax",
      asm"retn"
    )
  }
}

class HelloWorldTest64 extends FlatSpec with ShouldMatchers {
  
  val executableName = "test_HelloWorldTest64.exe"

  "A 64-bit Hello world" should "print 'Hello World'" in {
    getProgramOutput(HelloWorld64, true) should equal("Hello World!")
  }
}