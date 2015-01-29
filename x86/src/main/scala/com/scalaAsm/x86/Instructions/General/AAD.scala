package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: ASCII Adjust AX Before Division
// Category: general/arith/decimal

object AAD extends InstructionDefinition[OneOpcode]("AAD") with AADImpl

trait AADImpl {
  implicit object AAD_0 extends AAD._0 {
    def opcode = 0xD5
    override def hasImplicitOperand = true
  }
}
