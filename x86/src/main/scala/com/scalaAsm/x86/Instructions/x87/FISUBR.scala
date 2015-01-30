package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Reverse Subtract
// Category: general/arith

object FISUBR extends InstructionDefinition[OneOpcode]("FISUBR") with FISUBRImpl

trait FISUBRImpl {
  implicit object FISUBR_0 extends FISUBR._1[m32] {
    def opcode = 0xDA /+ 5
    override def hasImplicitOperand = true
  }

  implicit object FISUBR_1 extends FISUBR._1[m16] {
    def opcode = 0xDE /+ 5
    override def hasImplicitOperand = true
  }
}
