package com.scalaAsm

import com.scalaAsm.assembler.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.CodeSection
import com.scalaAsm.x86.Operands.addr

object FlagTest extends AsmProgram {
  
  import com.scalaAsm.x86.Instructions.Standard._
  
  dataSections += new DataSection {
    builder += Variable("rckeep", "\0\0\0\0\0\0\0\0")
    builder += Variable("flagTest", "Running flag test!\r\n\u0000")
    builder += Variable("pressAnyKey", "Press any key to continue ...\r\n\u0000")
  }

  codeSections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)

    procedure(name = "start",
      push(STD_OUTPUT_HANDLE),
      call("GetStdHandle"),
      mov(ebx, eax),
      push(byte(0)),
      push(byte(0)),
      push(byte(0x14)),
      push("flagTest"),
      push(eax),
      call("WriteFile"),
      push(STD_OUTPUT_HANDLE),
      call("GetStdHandle"),
      mov(ebx, eax),
      pushf(()),
      pop(eax), // flags now in eax
      mov(ecx, addr("rckeep")),
      mov(*(ecx+byte(0)), eax),
      push(byte(0)),
      push(byte(0)),
      push(byte(0x8)),
      push("rckeep"),
      push(eax),
      call("WriteFile"),
      push(STD_INPUT_HANDLE),
      call("GetStdHandle"),
      push(eax),
      call("FlushConsoleInputBuffer"),
      call("_getch"),
      mov(eax, dword(0)),
      retn(())
    )
  }
}