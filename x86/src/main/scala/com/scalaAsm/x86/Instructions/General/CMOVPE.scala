package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Conditional Move - parity/parity even (PF=1)
// Category: general/datamov

object CMOVPE extends InstructionDefinition[OneOpcode]("CMOVPE") with CMOVPEImpl

trait CMOVPEImpl {
  implicit object CMOVPE_0 extends CMOVPE._2[r16, rm16] {
    def opcode = 0x4A /r
  }

  implicit object CMOVPE_1 extends CMOVPE._2[r32, rm32] {
    def opcode = 0x4A /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object CMOVPE_2 extends CMOVPE._2[r64, rm64] {
    def opcode = 0x4A /r
    override def prefix = REX.W(true)
  }
}
