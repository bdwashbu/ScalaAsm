package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32

object rdtscExample extends AsmProgram[x86_32] {
  
  import com.scalaAsm.x86.Instructions.General._
  import com.scalaAsm.x86.Instructions.System._
  
  sections += new DataSection (
    Variable("numClocks", "Number of clock cycles = %d\n\u0000")
  ) {}

  sections += new CodeSection {

    builder += Code( 
      xor(ecx, ecx),
      rdtsc(()),
      add(ecx, eax),
      rdtsc(()),
      sub(eax,ecx),
      push(eax),
      push("numClocks"),
      call("printf"),
      pop(eax),
      pop(eax),
      call("_getch"),
      retn(())
    )
  }
}