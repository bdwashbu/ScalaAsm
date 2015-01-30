package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_64

object HelloWorldx87 extends AsmProgram[x86_64] {
  
  import com.scalaAsm.x86.Instructions.General._
  import com.scalaAsm.x86.Instructions.x87._
  
  sections += new DataSection (
    Variable("helloWorld", "Hello World!\r\n\u0000"),
    Variable("pressAnyKey", "Press any key to continue ...\u0000"),
    Variable("calc_str_x87", "%.0f plus %.0f equals %.0f according to the x87 FPU.\u0000"),
    Variable("a", "        "),
    Variable("b", "        "),
    Variable("result", "        ")
  ) {}

  sections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)

    builder += Code(
      push(rbp),
      mov(rbp, rsp),
      xor(rax, rax),
      mov(rdi, addr64("helloWorld")),
      call("printf"),
      fld(addr64("a")),
      fld(addr64("b")),
      //fadd
      fstp(addr64("result")),
      mov(rdi, addr64("calc_str_x87")),
      mov(rax, qword(3)),
      
      retn(())
    )
  }
}