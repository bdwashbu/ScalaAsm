package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.{x86_32, x86_64}
import com.scalaAsm.asm._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object HelloWorld2 extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.General._
  
  sections += new DataSection (
    Variable("helloWorld", "Hello World!\r\n\u0000"),
    Variable("pressAnyKey", "Press any key to continue ...\u0000")
  ) {}

  sections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = qword(-11)
    val STD_INPUT_HANDLE = qword(-10)

    procedure(name = "start",
      List(PUSH(rsp)),
      asm"push [rsp]",
      List(AND(spl, byte(0xF0))),
      List(MOV(rdx, dword(0xE))),
      List(LEA(rcx, addr("helloWorld"))),
      List(SUB(rsp, byte(0x20))),
      asm"invoke printf", // needs work
      List(LEA(rsp, rsp + byte(0x28))),
      List(POP(rsp)),
      List(PUSH(rsp)),
      asm"push [rsp]",
      List(AND(spl, byte(0xF0))),
      List(MOV(rdx, dword(0x1D))),
      List(LEA(rcx, addr("pressAnyKey"))),
      List(SUB(rsp, byte(0x20))),
      asm"invoke printf", // needs work
      List(LEA(rsp, rsp + byte(0x28))),
      List(POP(rsp)),
      List(PUSH(rsp)),
      asm"push [rsp]",
      List(AND(spl, byte(0xF0))),
      List(MOV(rcx, STD_INPUT_HANDLE)),
      List(SUB(rsp, byte(0x20))),
      asm"invoke GetStdHandle", // needs work
      List(LEA(rsp, rsp + byte(0x28))),
      List(POP(rsp)),
      List(PUSH(rsp)),
      asm"push [rsp]",
      List(AND(spl, byte(0xF0))),
      List(MOV(rcx, rax)),
      List(SUB(rsp, byte(0x20))),
      asm"invoke FlushConsoleInputBuffer", // needs work
      List(LEA(rsp, rsp + byte(0x28))),
      List(POP(rsp)),
      List(PUSH(rsp)),
      asm"push [rsp]",
      List(AND(spl, byte(0xF0))),
      List(SUB(rsp, byte(0x20))),
      asm"invoke _getch", // needs work
      List(LEA(rsp, rsp + byte(0x28))),
      List(POP(rsp)),
      List(XOR(rax, rax)),
      List(RETN(()))
    )
  }
}