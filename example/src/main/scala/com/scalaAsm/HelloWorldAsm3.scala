package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection

object HelloWorld3 extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.Standard._

  dataSections += new DataSection {
    builder += Variable("pressAnyKey", "Press any key to continue ...\0")
    builder += Variable("newline", "\r\n\0")
    builder += Variable("helloWorld", "Hello World!\n\0")
  }

  codeSections += new CodeSection {

    procedure(name = "start",
      call("printHelloWorld"),
      push("pressAnyKey"),
      call("flushBuffer"),
      call("waitForKeypress"),
      push("newline"),
      call("flushBuffer"),
      push(byte(0)),
      call("ExitProcess"))

    procedure(name = "printHelloWorld",
      push("helloWorld"),
      call("printf"),
      add(esp, byte(4)),
      retn(()))
      
    val numberOfBytesToWrite = *(ebp + byte(-12))
    val numberOfBytesWritten = *(ebp + byte(-8))
    val hFile = *(ebp + byte(-4))
    val lpBuffer = *(ebp + byte(8))
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)
    
    procedure(name = "flushBuffer",
      push(ebp),
      mov(ebp, esp),
      add(esp, byte(-12)),
      push(STD_OUTPUT_HANDLE),
      call("GetStdHandle"),
      mov(hFile, eax),
      push(lpBuffer),
      call("strlen"),
      mov(numberOfBytesToWrite, eax),
      push(byte(0)),
      lea(eax, numberOfBytesWritten),
      push(eax),
      push(numberOfBytesToWrite),
      push(lpBuffer),
      push(hFile),
      call("WriteFile"),
      mov(eax, numberOfBytesWritten),
      leave(()),
      retn(word(4)))

    procedure(name = "waitForKeypress",
      push(STD_INPUT_HANDLE),
      call("GetStdHandle"),
      push(eax),
      call("FlushConsoleInputBuffer"),
      push(byte(1)),
      call("Sleep"),
      call("_kbhit"),
      test(eax, eax), // eax is 0 if a key has not been pressed
      jz(byte(-16)), // if a key has not been pressed, loop around again
      call("_getch"),
      retn(()))

    procedure(name = "strlen",
      mov(eax, *(esp + byte(4))), // pointer to string
      lea(edx, *(eax + byte(3))),
      push(ebp),
      push(edi),
      mov(ebp, dword(0x80808080)),

      label("start"),

      repeat(3, List(
          mov(edi, *(eax)), // read first 4 bytes
          add(eax, byte(4)), // increment pointer
          lea(ecx, *(edi - dword(0x1010101))), // subtract 1 from each byte
          not(edi), // invert all bytes
          and(ecx, edi),
          and(ecx, ebp),
          jnz("test"))),

      mov(edi, *(eax)),
      add(eax, byte(4)),
      lea(ecx, *(edi - dword(0x1010101))),
      not(edi),
      and(ecx, edi),
      and(ecx, ebp),
      jz("start"),

      label("test"),
      test(ecx, dword(0x8080)), // test first 2 bytes
      jnz("end"),
      shr(ecx, byte(0x10)),
      add(eax, byte(2)),
      label("end"),
      shl(cl, One),
      sbb(eax, edx), // compute length
      pop(edi),
      pop(ebp),
      retn(word(4)))

    builder += align(2)
  }
}