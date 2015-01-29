package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: ASCII Adjust AX After Multiply
// Category: general/arith/decimal

object AAM extends InstructionDefinition[OneOpcode]("AAM") with AAMImpl

trait AAMImpl {
  implicit object AAM_0 extends AAM._0 {
    def opcode = 0xD4
    override def hasImplicitOperand = true
  }
}
