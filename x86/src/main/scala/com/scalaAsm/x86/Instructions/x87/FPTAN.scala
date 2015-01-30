package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Partial Tangent
// Category: general/trans

object FPTAN extends InstructionDefinition[OneOpcode]("FPTAN") with FPTANImpl

trait FPTANImpl {
  implicit object FPTAN_0 extends FPTAN._0 {
    def opcode = 0xD9 /+ 6
    override def hasImplicitOperand = true
  }
}
