package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Change Sign
// Category: general/arith

object FCHS extends InstructionDefinition[OneOpcode]("FCHS") with FCHSImpl

trait FCHSImpl {
  implicit object FCHS_0 extends FCHS._0 {
    def opcode = 0xD9 /+ 4
    override def hasImplicitOperand = true
  }
}
