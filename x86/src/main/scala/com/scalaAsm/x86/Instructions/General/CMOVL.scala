package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Conditional Move - less/not greater (SF!=OF)
// Category: general/datamov

object CMOVL extends InstructionDefinition[OneOpcode]("CMOVL") with CMOVLImpl

trait CMOVLImpl {
  implicit object CMOVL_0 extends CMOVL._2[r16, rm16] {
    def opcode = 0x4C /r
  }

  implicit object CMOVL_1 extends CMOVL._2[r32, rm32] {
    def opcode = 0x4C /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object CMOVL_2 extends CMOVL._2[r64, rm64] {
    def opcode = 0x4C /r
    override def prefix = REX.W(true)
  }
}
