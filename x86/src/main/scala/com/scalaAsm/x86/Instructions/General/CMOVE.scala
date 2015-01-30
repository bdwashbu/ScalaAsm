package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Conditional Move - zero/equal (ZF=0)
// Category: general/datamov

object CMOVE extends InstructionDefinition[OneOpcode]("CMOVE") with CMOVEImpl

trait CMOVEImpl {
  implicit object CMOVE_0 extends CMOVE._2[r16, rm16] {
    def opcode = 0x44 /r
  }

  implicit object CMOVE_1 extends CMOVE._2[r32, rm32] {
    def opcode = 0x44 /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object CMOVE_2 extends CMOVE._2[r64, rm64] {
    def opcode = 0x44 /r
    override def prefix = REX.W(true)
  }
}
