package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Bit Scan Forward
// Category: general/bit

object BSF extends InstructionDefinition[OneOpcode]("BSF") with BSFImpl

trait BSFImpl {
  implicit object BSF_0 extends BSF._2[r16, rm16] {
    def opcode = 0xBC /r
  }

  implicit object BSF_1 extends BSF._2[r32, rm32] {
    def opcode = 0xBC /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object BSF_2 extends BSF._2[r64, rm64] {
    def opcode = 0xBC /r
    override def prefix = REX.W(true)
  }
}
