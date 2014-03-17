package com.scalaAsm.x86

package object Instructions {
  import x86Registers._
  
  type imm8 = Immediate8
  type imm16 = Immediate16
  type imm32 = Immediate32
  
  type rm = RegisterOrMemory
  type rm8 = rm { type Size = ByteOperand }
  type rm16 = rm { type Size = WordOperand }
  type rm32 = rm { type Size = DwordOperand }
  type rm64 = rm { type Size = QwordOperand }
  
  type r8 = Register8 with RegisterID
  type r16 = Register16 with RegisterID
  type r32 = Register32 with RegisterID
  type r64 = Register64 with RegisterID
  
  trait One
}