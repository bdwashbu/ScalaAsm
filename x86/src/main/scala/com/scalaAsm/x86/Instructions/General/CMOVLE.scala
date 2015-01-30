package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Conditional Move - less or equal/not greater ((ZF=1) OR (SF!=OF))
// Category: general/datamov

object CMOVLE extends InstructionDefinition[OneOpcode]("CMOVLE") with CMOVLEImpl

trait CMOVLEImpl {
  implicit object CMOVLE_0 extends CMOVLE._2[r16, rm16] {
    def opcode = 0x4E /r
  }

  implicit object CMOVLE_1 extends CMOVLE._2[r32, rm32] {
    def opcode = 0x4E /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object CMOVLE_2 extends CMOVLE._2[r64, rm64] {
    def opcode = 0x4E /r
    override def prefix = REX.W(true)
  }
}
