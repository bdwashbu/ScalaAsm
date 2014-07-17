package com.scalaAsm

import com.scalaAsm.assembler.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.CodeSection

object HelloWorld extends AsmProgram {
  
  dataSections += new DataSection {
    builder += Variable("helloWorld", "Hello World!\r\n\u0000")
    builder += Variable("pressAnyKey", "Press any key to continue ...\r\n\u0000")
  }

  codeSections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)

    procedure(name = "start",
      push(STD_OUTPUT_HANDLE),
      call("GetStdHandle"),
      mov(ebx, eax),
      push(byte(0)),
      push(byte(0)),
      push(byte(0x0E)),
      push("helloWorld"),
      push(eax),
      call("WriteFile"),
      push(byte(0)),
      push(byte(0)),
      push(byte(0x1D)),
      push("pressAnyKey"),
      push(ebx),
      call("WriteFile"),
      push(STD_INPUT_HANDLE),
      call("GetStdHandle"),
      push(eax),
      call("FlushConsoleInputBuffer"),
      call("_getch"),
      mov(eax, dword(0)),
      retn
    )
  }
}