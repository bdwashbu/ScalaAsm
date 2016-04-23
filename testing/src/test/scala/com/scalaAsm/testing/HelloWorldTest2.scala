package com.scalaAsm.testing

import org.scalatest._
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import com.scalaAsm.asm._
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32
import com.scalaAsm.x86.Instructions._
import com.scalaAsm.x86._
import scala.language.experimental.macros
import com.scalaAsm.x86.Operands.Memory.Indirect


object HelloWorld2Test extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.General._

  sections += new DataSection (
    Variable("helloWorld", "Hello World!\n\u0000")
  ) {}

  sections += new CodeSection {

    procedure(name = "start",asm"""
      call printHelloWorld
      push ebx
      call flushBuffer
      retn""")

    procedure(name = "printHelloWorld",asm"""
      push helloWorld
      call printf
      add esp, 4
      retn""")
      
    val numberOfBytesToWrite = ebp + byte(-12)
    val numberOfBytesWritten = ebp + byte(-8)
    val hFile = "[ebp - 4]"
    val lpBuffer = ebp + byte(8)
    val STD_OUTPUT_HANDLE = "byte -14"
    val STD_INPUT_HANDLE = byte(-10)
   
    procedure(name = "flushBuffer",asm"""
      push ebp
      mov ebp, esp
      add esp, -12
      call GetStdHandle
      mov $hFile, eax
      push $lpBuffer
      call strlen
      mov $numberOfBytesToWrite, eax
      push byte 0
      lea eax, $numberOfBytesWritten
      push eax
      push $numberOfBytesToWrite
      push $lpBuffer
      push $hFile
      call WriteFile
      mov eax, $numberOfBytesWritten
      leave""",
      List(RETN(word(4))))

    procedure(name = "strlen",asm"""
      mov eax, [esp + 4] // pointer to string
      lea eax, [esp + 3]
      push ebp
      push edi
      mov ebp, 0x80808080""",

      asm"start:",

      repeat(3,asm"""
          mov edi, [eax] // read first 4 bytes
          ADD eax, 4 // increment pointer
          lea ecx, [edi - 0x1010101] // subtract 1 from each byte
          not edi // invert all bytes
          and ecx, edi
          and ecx, ebp
          jnz test"""),

      asm"""
      mov edi, [eax]
      add eax, 4
      lea ecx, [edi - 0x1010101]
      not edi
      and ecx, edi
      and ecx, ebp
      jz start
      test:
      test ecx, 0x8080 // test first 2 bytes
      jnz end
      shr ecx, 16
      add eax, 2
      end:
      shl cl
      sbb eax, edx // compute length
      pop edi
      pop ebp""",
      List(RETN(word(4))))

    builder += align(2)
  }
}

class HelloWorldTest2 extends FlatSpec with ShouldMatchers {
  
  val executableName = "test_HelloWorldTest2.exe"

  "A complex 32-bit Hello world" should "print 'Hello World'" in {
    getProgramOutput(HelloWorld2Test, false) should equal("Hello World!")
  }
}