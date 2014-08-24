package com.scalaAsm

import com.scalaAsm.assembler.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.CodeSection

object HelloWorld2 extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.Standard._
  
  dataSections += new DataSection {
    builder += Variable("helloWorld", "Hello World!\r\n\0")
    builder += Variable("pressAnyKey", "Press any key to continue ...\r\n\0")
  }

  codeSections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = qword(-11)
    val STD_INPUT_HANDLE = byte(-10)

    procedure(name = "start",
      push(rsp),
      push(*(rsp)),
      and(spl, byte(0xF0)),
      mov(rcx, STD_OUTPUT_HANDLE),
      sub(rsp, byte(0x20)),
      call("GetStdHandle"),
      lea(rsp, *(rsp + byte(0x28))),
      pop(rsp),
      mov(rbx,rax),
      push(rsp),
      push(*(rsp)),
      or(spl, byte(0x8)),
      push(byte(0x0)),
      //lea(r9, "helloWorld"),
      //mov(rcx, rax)
      
      mov(ebx, eax),
      push(byte(0)),
      push(byte(0)),
      push(byte(0x0E)),
      push("helloWorld"),
      push(eax),
      call("WriteFile"),
      push(byte(0)),
      push(byte(0)),
      push(byte(0x1D)),
      push("pressAnyKey"),
      push(ebx),
      call("WriteFile"),
      push(STD_INPUT_HANDLE),
      call("GetStdHandle"),
      push(eax),
      call("FlushConsoleInputBuffer"),
      call("_getch"),
      mov(eax, dword(0)),
      retn
    )
  }
}