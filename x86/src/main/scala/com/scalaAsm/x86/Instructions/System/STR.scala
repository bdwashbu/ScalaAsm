package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store Task Register
// Category: general

object STR extends InstructionDefinition[OneOpcode]("STR") with STRImpl

trait STRImpl {
  implicit object STR_0 extends STR._1[m16] {
    def opcode = 0x0 /+ 1
    override def hasImplicitOperand = true
  }

  implicit object STR_1 extends STR._1[r16] {
    def opcode = 0x0 /+ 1
    override def hasImplicitOperand = true
  }

  implicit object STR_2 extends STR._1[r32] {
    def opcode = 0x0 /+ 1
    override def hasImplicitOperand = true
  }

  implicit object STR_3 extends STR._1[r64] {
    def opcode = 0x0 /+ 1
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}
