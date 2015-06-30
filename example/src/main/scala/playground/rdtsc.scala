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
      List(RDTSCP(())),
      List(PUSH(edx)),
      List(PUSH(eax)),
      List(CPUID(())),
      List(RDTSCP(())),
      List(PUSH(edx)),
      List(PUSH(eax)),
      List(CPUID(())),
      List(POP(edx)),
      List(POP(edx)),
      List(POP(eax)),
      List(POP(eax)),
      List(SUB(eax,edx)),
      List(PUSH(eax)),
      asm"push numClocks",
      asm"call printf",
      List(POP(eax)),
      List(POP(eax)),
      asm"call _getch",
      List(RETN(()))
    )
  }
}