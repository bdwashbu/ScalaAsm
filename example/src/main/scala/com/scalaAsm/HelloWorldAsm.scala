package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.x86.Addressing._
import com.scalaAsm.x86.rm32

object HelloWorld extends AsmProgram {

  val data = new Data {
    val pressAnyKey = "Press any key to continue ...\0"
    val newline = "\r\n\0"
    val helloWorld = "Hello World!\n\0"
  }

  val code = new Code {

    proc("start") {
      call("printHelloWorld")
      push("pressAnyKey")
      call("flushBuffer")
      call("waitForKeypress")
      push("newline")
      call("flushBuffer")
      push(imm8(0))
      call("ExitProcess")
    }

    proc("printHelloWorld") {
      push("helloWorld")
      call("printf")
      add(esp, imm8(4))
      retn
    }

    proc("flushBuffer") {

      val numberOfBytesToWrite = new rm32{ val reg = ebp; val offset8 = -12.toByte; val isMemory = true}
      val numberOfBytesWritten = new rm32{ val reg = ebp; val offset8 = -8.toByte; val isMemory = true}
      val hFile = new rm32{ val reg = ebp; val offset8 = -4.toByte; val isMemory = true}
      val lpBuffer = new rm32{ val reg = ebp; val offset8 = 8.toByte; val isMemory = true}
      val STD_OUTPUT_HANDLE = imm8(-11)

      push(ebp)
      mov(ebp, esp)
      add(esp, imm8(-12))
      push(STD_OUTPUT_HANDLE)
      call("GetStdHandle")
      mov(hFile, eax)
      push(lpBuffer)
      call("strlen")
      mov(numberOfBytesToWrite, eax)
      push(imm8(0))
      lea(eax, numberOfBytesWritten)
      push(eax)
      push(numberOfBytesToWrite)
      push(lpBuffer)
      push(hFile)
      call("WriteFile")
      mov(eax, numberOfBytesWritten)
      leave
      retn(imm16(4))
    }

    align(0x10)

    proc("waitForKeypress") {

      val STD_INPUT_HANDLE = imm8(-10)

      push(STD_INPUT_HANDLE)
      call("GetStdHandle")
      push(eax)
      call("FlushConsoleInputBuffer")
      push(imm8(1))
      call("Sleep")
      call("_kbhit")
      test(eax, eax)
      jz(imm8(-17))
      call("_getch")
      retn
    }

    align(0x10)

    proc("strlen") {

      mov(eax, new rm32{ val reg = esp; val offset8 = 4.toByte; val isMemory = true}) //*(esp + imm8(4))) // pointer to string
      lea(edx, new rm32{ val reg = eax; val offset8 = 3.toByte; val isMemory = true}) //*(eax + imm8(3)))
      push(ebp)
      push(edi)
      mov(ebp, imm32(0x80808080))

      label("start")

      for (i <- 0 until 3) {
        mov(edi, *(eax)) // read first 4 bytes
        add(eax, imm8(4)) // increment pointer
        lea(ecx, *(edi - imm32(0x1010101))) // subtract 1 from each byte
        not(edi) // invert all bytes
        and(ecx, edi)
        and(ecx, ebp)
        jnz("test")
      }

      mov(edi, *(eax))
      add(eax, imm8(4))
      lea(ecx, *(edi - imm32(0x1010101)))
      not(edi)
      and(ecx, edi)
      and(ecx, ebp)
      jz("start")

      label("test")
      test(ecx, imm32(0x8080)) // test first 2 bytes
      jnz("end")
      shr(ecx, imm8(0x10))
      add(eax, imm8(2))
      label("end")
      shl(cl)
      sbb(eax, edx) // compute length
      pop(edi)
      pop(ebp)
      retn(imm16(4))
    }

    align(2)

    proc("ExitProcess") {
      jmp("ExitProcess")
    }

    proc("GetStdHandle") {
      jmp("GetStdHandle")
    }

    proc("WriteFile") {
      jmp("WriteFile")
    }

    proc("FlushConsoleInputBuffer") {
      jmp("FlushConsoleInputBuffer")
    }

    proc("Sleep") {
      jmp("Sleep")
    }
  }
}