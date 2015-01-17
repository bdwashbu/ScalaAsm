package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CALL extends InstructionDefinition[OneOpcode]("CALL") with CALLImpl

// Call Procedure

trait CALLLow {
  implicit object CALL_0 extends CALL._1[rm16] {
    def opcode = 0xFF /+ 2
    override def hasImplicateOperand = true
  }

  implicit object CALL_1 extends CALL._1[rm32] {
    def opcode = 0xFF /+ 2
    override def hasImplicateOperand = true
  }

  implicit object CALL_2 extends CALL._1[rm64] {
    def opcode = 0xFF /+ 2
    override def hasImplicateOperand = true
  }
}

trait CALLImpl extends CALLLow {
  implicit object CALL_3 extends CALL._1[rel16] {
    def opcode = 0xE8
    override def hasImplicateOperand = true
  }

  implicit object CALL_4 extends CALL._1[rel32] {
    def opcode = 0xE8
    override def hasImplicateOperand = true
  }
}
