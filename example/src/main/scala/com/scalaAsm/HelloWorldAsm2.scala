package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Addressing._
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.CodeSection

object HelloWorld2 extends AsmProgram {
  
  dataSegments += new DataSection {
    builder += Variable("RCKEEP", "", 0)
    builder += Variable("helloWorld", "Hello World!\r\n\0", "Hello World!\r\n\0".length)
    builder += Variable("pressAnyKey", "Press any key to continue ...\r\n\0", "Press any key to continue ...\r\n\0".length)
  }

  codeSegments += new CodeSection {

    builder += Procedure("start", List(
      push(imm32(1)),
      callNear(*(imm32(0x403000)).rel32),
      mov(ebx, eax),
      push("RCKEEP")
    ))
  }
}