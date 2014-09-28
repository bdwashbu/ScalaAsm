package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.x86.Operands.addr

object FlagTest extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.Standard._
  
  dataSections += new DataSection {
    builder += Variable("rckeep", "\0\0\0\0\0\0\0\0")
    builder += Variable("flagTest", "Running flag test!\r\n\u0000")
    builder += Variable("result", "flag value: %x\r\n\u0000")
  }

  codeSections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)

    procedure(name = "start",
      push("flagTest"),
      call("printf"),
      pop(ebx),
      pushf(()),
      push("result"),
      call("printf"),
      pop(ebx),
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