package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Signed Multiply
// Category: general/arith/binary

object IMUL extends InstructionDefinition[OneOpcode]("IMUL") with IMULImpl

trait IMULImpl {
  implicit object IMUL_0 extends IMUL._1[rm8] {
    def opcode = 0xF6 /+ 5
    override def hasImplicitOperand = true
  }

  implicit object IMUL_1 extends IMUL._1[rm16] {
    def opcode = 0xF7 /+ 5
    override def hasImplicitOperand = true
  }

  implicit object IMUL_2 extends IMUL._1[rm32] {
    def opcode = 0xF7 /+ 5
    override def hasImplicitOperand = true
  }

  implicit object IMUL_3 extends IMUL._1[rm64] {
    def opcode = 0xF7 /+ 5
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}
