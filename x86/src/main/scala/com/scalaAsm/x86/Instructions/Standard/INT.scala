package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object INT extends InstructionDefinition[OneOpcode]("INT") with INTImpl

// Call to Interrupt Procedure

trait INTImpl {
  implicit object INT_0 extends INT._1[imm8] {
    def opcode = 0xCD
    override def hasImplicateOperand = true
  }
}
