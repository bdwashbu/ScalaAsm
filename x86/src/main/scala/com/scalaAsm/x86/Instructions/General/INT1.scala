package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Call to Interrupt Procedure
// Category: general/breakstack

object INT1 extends InstructionDefinition[OneOpcode]("INT1") with INT1Impl

trait INT1Impl {
  implicit object INT1_0 extends INT1._0 {
    def opcode = 0xF1
    override def hasImplicitOperand = true
  }
}
