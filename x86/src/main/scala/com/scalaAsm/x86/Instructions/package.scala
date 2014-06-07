package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Memory.AddressingMode

trait One extends Operand {
  def size = 1
}

package object Instructions {

  type imm8 = Immediate8
  type imm16 = Immediate16
  type imm32 = Immediate32
  type imm64 = Immediate64
  
  type Operand = com.scalaAsm.x86.Operands.Operand
  
  //type AddressingFormSpecifier = Operands.AddressingFormSpecifier
  
  type rm = RegisterOrMemory
  type rm8 = rm { type Size = ByteOperand }
  type rm16 = rm { type Size = WordOperand }
  type rm32 = rm { type Size = DwordOperand }
  type rm64 = rm { type Size = QwordOperand }
  
  type m16 = AddressingMode { type Size = WordOperand }
  
  type rel16 = Relative { type Size = WordOperand }
  type rel32 = Relative { type Size = DwordOperand }
  
  type r = GPR
  type r8 = Register8 with GeneralPurpose
  type r16 = Register16 with GeneralPurpose
  type r32 = Register32 with GeneralPurpose
  type r64 = Register64 with GeneralPurpose

}