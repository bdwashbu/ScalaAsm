package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection

object HelloWorld extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.Standard._
  
  dataSections += new DataSection {
    builder += Variable("helloWorld", "Hello World!\r\n\u0000")
    builder += Variable("pressAnyKey", "Press any key to continue ...\u0000")
  }

  codeSections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)

    procedure(name = "start",
      push("helloWorld"),
      call("printf"),
      pop(ebx),
      push("pressAnyKey"),
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