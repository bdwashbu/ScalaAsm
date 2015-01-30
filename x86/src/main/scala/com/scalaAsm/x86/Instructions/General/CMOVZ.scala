package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Conditional Move - zero/equal (ZF=0)
// Category: general/datamov

object CMOVZ extends InstructionDefinition[OneOpcode]("CMOVZ") with CMOVZImpl

trait CMOVZImpl {
  implicit object CMOVZ_0 extends CMOVZ._2[r16, rm16] {
    def opcode = 0x44 /r
  }

  implicit object CMOVZ_1 extends CMOVZ._2[r32, rm32] {
    def opcode = 0x44 /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object CMOVZ_2 extends CMOVZ._2[r64, rm64] {
    def opcode = 0x44 /r
    override def prefix = REX.W(true)
  }
}
