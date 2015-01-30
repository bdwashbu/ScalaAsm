package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Bit Scan Reverse
// Category: general/bit

object BSR extends InstructionDefinition[OneOpcode]("BSR") with BSRImpl

trait BSRImpl {
  implicit object BSR_0 extends BSR._2[r16, rm16] {
    def opcode = 0xBD /r
  }

  implicit object BSR_1 extends BSR._2[r32, rm32] {
    def opcode = 0xBD /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object BSR_2 extends BSR._2[r64, rm64] {
    def opcode = 0xBD /r
    override def prefix = REX.W(true)
  }
}
