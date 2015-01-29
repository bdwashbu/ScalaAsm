package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Call to Interrupt Procedure
// Category: general/breakstack

object INT extends InstructionDefinition[OneOpcode]("INT") with INTImpl

trait INTImpl {
  implicit object INT_0 extends INT._1[imm8] {
    def opcode = 0xCD
    override def hasImplicitOperand = true
  }
}
