package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Addressing._
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.CodeSection

object HelloWorld extends AsmProgram {
  
  dataSegments += new DataSection {
    builder += Variable("pressAnyKey", "Press any key to continue ...\0", "Press any key to continue ...\0".length)
    builder += Variable("newline", "\r\n\0", "\r\n\0".length)
    builder += Variable("helloWorld", "Hello World!\n\0", "Hello World!\n\0".length)
  }

  codeSegments += new CodeSection {

    builder += Procedure("start", List(
      call("printHelloWorld"),
      push("pressAnyKey"),
      call("flushBuffer"),
      call("waitForKeypress"),
      push("newline"),
      call("flushBuffer"),
      push(imm8(0)),
      call("ExitProcess")
    ))

    builder += Procedure("printHelloWorld", List(
      push("helloWorld"),
      call("printf"),
      add(esp, imm8(4)),
      retn
    ))

    val numberOfBytesToWrite = *(ebp - byte(12))
    val numberOfBytesWritten = *(ebp + byte(-8))
    val hFile = *(ebp + byte(-4))
    val lpBuffer = *(ebp + byte(8))
    //val STD_OUTPUT_HANDLE = imm8(-11)
    
    builder += Procedure("flushBuffer", List(
      push(ebp),
      mov(ebp, esp),
      add(esp, imm8(-12)),
      push(imm8(-11)),
      call("GetStdHandle"),
      mov(hFile, eax),
      push(lpBuffer),
      call("strlen"),
      mov(numberOfBytesToWrite, eax),
      push(imm8(0)),
      lea(eax, numberOfBytesWritten),
      push(eax),
      push(numberOfBytesToWrite),
      push(lpBuffer),
      push(hFile),
      call("WriteFile"),
      mov(eax, numberOfBytesWritten),
      leave,
      retn(imm16(4))
    ))

    builder += align(0x10)

    builder += Procedure("waitForKeypress", List(
      push(imm8(-10)),
      call("GetStdHandle"),
      push(eax),
      call("FlushConsoleInputBuffer"),
      push(imm8(1)),
      call("Sleep"),
      call("_kbhit"),
      test(eax, eax),
      jz(imm8(-17)),
      call("_getch"),
      retn
    ))

    builder += align(0x10)
    
    builder += Procedure("strlen", List(
      mov(eax, *(esp + byte(4))),
      lea(edx, *(eax + byte(3))),
      push(ebp),
      push(edi),
      mov(ebp, imm32(0x80808080)),
      label("start"),
      
      repeat(3, List(
	      mov(edi, *(eax)), // read first 4 bytes
		  add(eax, imm8(4)), // increment pointer
	      lea(ecx, *(edi - dword(0x1010101))), // subtract 1 from each byte
	      not(edi), // invert all bytes
	      and(ecx, edi),
	      and(ecx, ebp),
	      jnz("test")
      )),
      
      mov(edi, *(eax)),
      add(eax, imm8(4)),
      lea(ecx, *(edi - dword(0x1010101))),
      not(edi),
      and(ecx, edi),
      and(ecx, ebp),
      jz("start"),

      label("test"),
      test(ecx, imm32(0x8080)), // test first 2 bytes
      jnz("end"),
      shr(ecx, imm8(0x10)),
      add(eax, imm8(2)),
      label("end"),
      shl(cl, One),
      sbb(eax, edx), // compute length
      pop(edi),
      pop(ebp),
      retn(imm16(4))
    ))

    builder += align(2)

    builder += Procedure("ExitProcess", List(jmp("ExitProcess")))
    builder += Procedure("GetStdHandle", List(jmp("GetStdHandle")))
    builder += Procedure("WriteFile", List(jmp("WriteFile")))
    builder += Procedure("FlushConsoleInputBuffer", List(jmp("FlushConsoleInputBuffer")))
    builder += Procedure("Sleep", List(jmp("Sleep")))
  }
}