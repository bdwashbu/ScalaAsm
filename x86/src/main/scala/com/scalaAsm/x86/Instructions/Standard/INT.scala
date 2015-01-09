package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object INT extends InstructionDefinition[OneOpcode]("INT") with INTImpl

trait INTImpl {
  implicit object INT_205_imm8 extends INT._1_new[imm8] {
    def opcode = 0xCD
    override def hasImplicateOperand = true
  }
}
