package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32

object HelloWorld extends AsmProgram[x86_32] {
  
  import com.scalaAsm.x86.Instructions.General._
  
  sections += new DataSection (
    Variable("helloWorld", "Hello World!\r\n\u0000"),
    Variable("pressAnyKey", "Press any key to continue ...\u0000")
  ) {}

  sections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)

    builder += Code(
      asm"push helloWorld",
      call("printf"),
      pop(ebx),
      asm"push pressAnyKey",
      call("printf"),
      pop(ebx),
      push(STD_INPUT_HANDLE),
      call("GetStdHandle"),
      push(eax),
      call("FlushConsoleInputBuffer"),
      call("_getch"),
      retn(())
    )
  }
}