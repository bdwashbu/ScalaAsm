package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32

object Factorial extends AsmProgram[x86_32] {
  
  import com.scalaAsm.x86.Instructions.Standard._

  sections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)

    builder += Code(
      push(ebp),
      mov(ebp, esp),
      mov(ebx, *(ebp+byte(8))),
      mov(eax, dword(1)),
      label("start"),
      cmp(ebx, byte(1)),
      jl(end),
      mul(ebx),
      dec(ebx),
      jmp("start"),
      label("end"),
      pop(ebp),
      retn(())
    )
  }
}