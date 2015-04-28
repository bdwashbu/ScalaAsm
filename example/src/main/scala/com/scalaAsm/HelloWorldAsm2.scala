package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.{x86_32, x86_64}
import com.scalaAsm.asm._

object HelloWorld2 extends AsmProgram[x86_64] {
  
  import com.scalaAsm.x86.Instructions.General._
  
  sections += new DataSection (
    Variable("helloWorld", "Hello World!\r\n\0"),
    Variable("pressAnyKey", "Press any key to continue ...\0")
  ) {}

  sections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = qword(-11)
    val STD_INPUT_HANDLE = qword(-10)

    procedure(name = "start",
      PUSH(rsp),
      PUSH(*(rsp)),
      AND(spl, byte(0xF0)),
      MOV(rdx, dword(0xE)),
      LEA(rcx, addr("helloWorld")),
      SUB(rsp, byte(0x20)),
      asm"invoke printf", // needs work
      LEA(rsp, rsp + byte(0x28)),
      POP(rsp),
      PUSH(rsp),
      PUSH(*(rsp)),
      AND(spl, byte(0xF0)),
      MOV(rdx, dword(0x1D)),
      LEA(rcx, addr("pressAnyKey")),
      SUB(rsp, byte(0x20)),
      asm"invoke printf", // needs work
      LEA(rsp, rsp + byte(0x28)),
      POP(rsp),
      PUSH(rsp),
      PUSH(*(rsp)),
      AND(spl, byte(0xF0)),
      MOV(rcx, STD_INPUT_HANDLE),
      SUB(rsp, byte(0x20)),
      asm"invoke GetStdHandle", // needs work
      LEA(rsp, rsp + byte(0x28)),
      POP(rsp),
      PUSH(rsp),
      PUSH(*(rsp)),
      AND(spl, byte(0xF0)),
      MOV(rcx, rax),
      SUB(rsp, byte(0x20)),
      asm"invoke FlushConsoleInputBuffer", // needs work
      LEA(rsp, rsp + byte(0x28)),
      POP(rsp),
      PUSH(rsp),
      PUSH(*(rsp)),
      AND(spl, byte(0xF0)),
      SUB(rsp, byte(0x20)),
      asm"invoke _getch", // needs work
      LEA(rsp, rsp + byte(0x28)),
      POP(rsp),
      XOR(rax, rax),
      RETN(())
    )
  }
}