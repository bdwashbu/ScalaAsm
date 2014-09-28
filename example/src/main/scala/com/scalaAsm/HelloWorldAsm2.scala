package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.x86.Operands.addr

object HelloWorld2 extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.Standard._
  
  dataSections += new DataSection {
    builder += Variable("rckeep", "\0\0\0\0")
    builder += Variable("helloWorld", "Hello World!\r\n\0")
    builder += Variable("pressAnyKey", "Press any key to continue ...\0")
  }

  codeSections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = qword(-11)
    val STD_INPUT_HANDLE = qword(-10)

    procedure(name = "start",
      push(rsp),
      push(*(rsp)),
      and(spl, byte(0xF0)),
      mov(rdx, dword(0xE)),
      lea(rcx, addr("helloWorld")),
      sub(rsp, byte(0x20)),
      invoke("printf"), // needs work
      lea(rsp, *(rsp + byte(0x28))),
      pop(rsp),
      push(rsp),
      push(*(rsp)),
      and(spl, byte(0xF0)),
      mov(rdx, dword(0x1D)),
      lea(rcx, addr("pressAnyKey")),
      sub(rsp, byte(0x20)),
      invoke("printf"), // needs work
      lea(rsp, *(rsp+byte(0x28))),
      pop(rsp),
      push(rsp),
      push(*(rsp)),
      and(spl, byte(0xF0)),
      mov(rcx, STD_INPUT_HANDLE),
      sub(rsp, byte(0x20)),
      invoke("GetStdHandle"), // needs work
      lea(rsp, *(rsp+byte(0x28))),
      pop(rsp),
      push(rsp),
      push(*(rsp)),
      and(spl, byte(0xF0)),
      mov(rcx, rax),
      sub(rsp, byte(0x20)),
      invoke("FlushConsoleInputBuffer"), // needs work
      lea(rsp, *(rsp+byte(0x28))),
      pop(rsp),
      push(rsp),
      push(*(rsp)),
      and(spl, byte(0xF0)),
      sub(rsp, byte(0x20)),
      invoke("_getch"), // needs work
      lea(rsp, *(rsp+byte(0x28))),
      pop(rsp),
      xor(rax, rax),
      retn(())
    )
  }
}