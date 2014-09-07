package com.scalaAsm

import com.scalaAsm.assembler.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.CodeSection
import com.scalaAsm.x86.Operands.addr

object HelloWorld2 extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.Standard._
  
  dataSections += new DataSection {
    builder += Variable("rckeep", "\0\0\0\0")
    builder += Variable("helloWorld", "Hello World!\r\n\0")
    builder += Variable("pressAnyKey", "Press any key to continue ...\r\n\0")
  }

  codeSections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = qword(-11)
    val STD_INPUT_HANDLE = byte(-10)

    procedure(name = "start",
      push(rsp),
      push(*(rsp+byte(0))), // weird fix for [ss:rsp] whatever that means
      and(spl, byte(0xF0)),
      mov(rcx, STD_OUTPUT_HANDLE),
      sub(rsp, byte(0x20)),
      invoke("GetStdHandle"),
      lea(rsp, *(rsp + byte(0x28))),
      pop(rsp),
      mov(rbx,rax),
      push(rsp),
      push(*(rsp+byte(0))),
      or(spl, byte(0x8)),
      push(byte(0x0)),
      lea(r9, addr("rckeep")),
      mov(r8, dword(0xE)),
      lea(rdx, addr("helloWorld")),
      mov(rcx, rax),
      sub(rsp, byte(0x20)),
      call("WriteFile"), // needs work
      lea(rsp, *(rsp + byte(0x28))),
      pop(rsp),
      push(rsp),
      push(*(rsp+byte(0))),
      or(spl, byte(0x8)),
      push(byte(0)),
      lea(r9, addr("rckeep")),
      mov(r8, dword(0x1D)),
      lea(rdx, addr("pressAnyKey")),
      mov(rcx, rbx),
      sub(rsp, byte(0x20)),
      call("WriteFile"), // needs work
      lea(rsp, *(rsp+byte(0x28))),
      pop(rsp),
      push(rsp),
      push(*(rsp+byte(0))),
      and(spl, byte(0xF0)),
      mov(rcx, qword(0x6)),
      sub(rsp, byte(0x20)),
      call("GetStdHandle"), // needs work
      lea(rsp, *(rsp+byte(0x28))),
      pop(rsp),
      push(rsp),
      push(*(rsp+byte(0))),
      and(spl, byte(0xF0)),
      mov(rcx, rax),
      sub(rsp, byte(0x20)),
      call("FlushConsoleInputBuffer"), // needs work
      lea(rsp, *(rsp+byte(0x28))),
      pop(rsp),
      push(rsp),
      push(*(rsp+byte(0))),
      and(spl, byte(0xF0)),
      sub(rsp, byte(0x20)),
      call("_getch"), // needs work
      lea(rsp, *(rsp+byte(0x28))),
      pop(rsp),
      xor(rax, rax),
      retn(())
    )
  }
}