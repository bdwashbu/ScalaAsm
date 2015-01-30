package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Conditional Move - not below/above or equal/not carry (CF=0)
// Category: general/datamov

object CMOVAE extends InstructionDefinition[OneOpcode]("CMOVAE") with CMOVAEImpl

trait CMOVAEImpl {
  implicit object CMOVAE_0 extends CMOVAE._2[r16, rm16] {
    def opcode = 0x43 /r
  }

  implicit object CMOVAE_1 extends CMOVAE._2[r32, rm32] {
    def opcode = 0x43 /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object CMOVAE_2 extends CMOVAE._2[r64, rm64] {
    def opcode = 0x43 /r
    override def prefix = REX.W(true)
  }
}
