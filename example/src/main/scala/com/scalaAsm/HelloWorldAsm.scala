package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Addressing._
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.CodeSection

object HelloWorld extends AsmProgram {
  
  dataSections += new DataSection {
    builder += Variable("helloWorld", "Hello World!\r\n\0")
    builder += Variable("pressAnyKey", "Press any key to continue ...\r\n\0")
  }

  codeSections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = imm8(-11)
    val STD_INPUT_HANDLE = imm8(-10)

    procedure(name = "start",
      push(STD_OUTPUT_HANDLE),
      call("GetStdHandle"),
      mov(ebx, eax),
      push(imm8(0)),
      push(imm8(0)),
      push(imm8(0x0E)),
      push("helloWorld"),
      push(eax),
      call("WriteFile"),
      push(imm8(0)),
      push(imm8(0)),
      push(imm8(0x1D)),
      push("pressAnyKey"),
      push(ebx),
      call("WriteFile"),
      push(STD_INPUT_HANDLE),
      call("GetStdHandle"),
      push(eax),
      call("FlushConsoleInputBuffer"),
      call("_getch"),
      mov(eax, imm32(0)),
      retn
    )
  }
}