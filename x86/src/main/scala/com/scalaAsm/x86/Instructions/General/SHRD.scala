package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Double Precision Shift Right
// Category: general/shftrot

object SHRD extends InstructionDefinition[OneOpcode]("SHRD") with SHRDImpl

trait SHRDImpl {
  implicit object SHRD_0 extends SHRD._2[rm16, r16] {
    def opcode = 0xAD /r
    override def hasImplicitOperand = true
  }

  implicit object SHRD_1 extends SHRD._2[rm32, r32] {
    def opcode = 0xAD /r
    override def explicitFormat(op1: rm32, op2: r32) = {
      if (op1.isInstanceOf[reg]) {
         Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op2, op1.asInstanceOf[reg])), immediate = None))
      } else None
    }
    override def hasImplicitOperand = true
  }

  implicit object SHRD_2 extends SHRD._2[rm64, r64] {
    def opcode = 0xAD /r
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}
