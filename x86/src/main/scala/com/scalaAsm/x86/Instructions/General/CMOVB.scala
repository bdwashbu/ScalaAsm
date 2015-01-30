package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Conditional Move - below/not above or equal/carry (CF=1)
// Category: general/datamov

object CMOVB extends InstructionDefinition[OneOpcode]("CMOVB") with CMOVBImpl

trait CMOVBImpl {
  implicit object CMOVB_0 extends CMOVB._2[r16, rm16] {
    def opcode = 0x42 /r
  }

  implicit object CMOVB_1 extends CMOVB._2[r32, rm32] {
    def opcode = 0x42 /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object CMOVB_2 extends CMOVB._2[r64, rm64] {
    def opcode = 0x42 /r
    override def prefix = REX.W(true)
  }
}
