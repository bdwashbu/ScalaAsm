package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32
import com.scalaAsm.asm._

object rdtscExample extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.General._
  import com.scalaAsm.x86.Instructions.System._
  
  sections += new DataSection (
    Variable("numClocks", "Number of clock cycles = %d\n\u0000")
  ) {}

  sections += new CodeSection {

    builder += Code( 
      RDTSCP(()),
      PUSH(edx),
      PUSH(eax),
      CPUID(()),
      RDTSCP(()),
      PUSH(edx),
      PUSH(eax),
      CPUID(()),
      POP(edx),
      POP(edx),
      POP(eax),
      POP(eax),
      SUB(eax,edx),
      PUSH(eax),
      asm"push numClocks",
      asm"call printf",
      POP(eax),
      POP(eax),
      asm"call _getch",
      RETN(())
    )
  }
}