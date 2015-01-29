package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Call Procedure
// Category: general/branchstack

object CALL extends InstructionDefinition[OneOpcode]("CALL") with CALLImpl

trait CALLLow {
  implicit object CALL_0 extends CALL._1[rm16] {
    def opcode = 0xFF /+ 2
    override def hasImplicitOperand = true
  }

  implicit object CALL_1 extends CALL._1[rm32] {
    def opcode = 0xFF /+ 2
    override def hasImplicitOperand = true
  }

  implicit object CALL_2 extends CALL._1[rm64] {
    def opcode = 0xFF /+ 2
    override def hasImplicitOperand = true
  }
}

trait CALLImpl extends CALLLow {
  implicit object CALL_3 extends CALL._1[rel16] {
    def opcode = 0xE8
    override def hasImplicitOperand = true
  }

  implicit object CALL_4 extends CALL._1[rel32] {
    def opcode = 0xE8
    override def hasImplicitOperand = true
  }
}
