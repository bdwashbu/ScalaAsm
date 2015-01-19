package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32

object Factorial extends AsmProgram[x86_32] {
  
  import com.scalaAsm.x86.Instructions.Standard._
  
  sections += new DataSection (
    Variable("test", "Test: %d\n\u0000")
  ) {}

  sections += new CodeSection {

    builder += Code(
      push("helloWorld"),
      call("printf"),
      pop(eax),
      mov(eax,dword(5)),
      mov(ebx,eax),
      label("Begin"),
      dec(ebx),
      test(ebx,ebx),
      je("End"),
      imul(ebx),
      jmp("Begin"),
      label("End"),
      push(eax),
      push("test"),
      call("printf"),
      pop(eax),
      pop(eax),
      call("_getch"),
      retn(())
    )
   
  }
}