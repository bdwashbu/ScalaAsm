package example

import org.scalatest._
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32
import com.scalaAsm.x86.Instructions._
import com.scalaAsm.x86._
import com.scalaAsm.asm._
import com.scalaAsm.x86.Operands.Memory.Indirect

object HelloWorld3 extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.General._

  sections += new DataSection (
    Variable("pressAnyKey", "Press any key to continue ...\n\u0000")
  ) {}

  sections += new CodeSection {

    procedure(name = "start", asm"""
      push pressAnyKey
      call flushBuffer
      push 0
      call ExitProcess
    """)
      
    val numberOfBytesToWrite = ebp + byte(-12)
    val numberOfBytesWritten = ebp + byte(-8)
    val hFile = ebp + byte(-4)
    val lpBuffer = "[ebp + 8]"
    val STD_OUTPUT_HANDLE = -11
    val STD_INPUT_HANDLE = -10
    
    procedure(name = "flushBuffer", asm"""
      push ebp
      mov ebp, esp // what
      add esp, byte -12
      push $STD_OUTPUT_HANDLE
      call GetStdHandle
      mov $hFile, eax
      push $lpBuffer
      call strlen
      mov $numberOfBytesToWrite, eax
      push 0
      lea eax, $numberOfBytesWritten
      push eax
      push $numberOfBytesToWrite
      push $lpBuffer
      push $hFile
      call WriteFile
      mov eax, $numberOfBytesWritten
      leave
      retn 4
      """)

    procedure(name = "strlen", asm"""
      mov eax, [esp + 4]
      lea edx, [eax + 3]
      push ebp
      push edi
      mov ebp, 0x80808080
      start:""",

      repeat(3, asm"""
          mov edi, [eax] // read first 4 bytes
          add eax, 4 // increment pointer
          lea ecx, [edi - 0x1010101] // subtract 1 from each byte
          not edi // invert all bytes
          and ecx, edi
          and ecx, ebp
          jnz test
        """),

      
      
      asm"""
      mov edi, [eax]
      add eax, 4
      lea ecx, [edi - 0x1010101]
      not edi
      and ecx, edi
      and ecx, ebp
      jz start
      test:
      test ecx, 0x8080
      jnz end
      shr ecx, byte 16
      add eax, 2
      end:
      shl cl
      sbb eax, edx
      pop edi
      pop ebp
      retn 4
      """
      )

    builder += align(2)
  }
}

class HelloWorldTest3 extends FlatSpec with ShouldMatchers {
  
  val executableName = "test_HelloWorldTest3.exe"

  "A test of flushbuffer" should "print 'Press any key to continue...'" in {
    getProgramOutput(HelloWorld3, false) should equal("Press any key to continue ...")
  }
}