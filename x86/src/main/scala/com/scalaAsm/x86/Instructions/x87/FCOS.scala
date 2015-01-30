package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Cosine
// Category: general/trans

object FCOS extends InstructionDefinition[OneOpcode]("FCOS") with FCOSImpl

trait FCOSImpl {
  implicit object FCOS_0 extends FCOS._0 {
    def opcode = 0xD9 /+ 7
    override def hasImplicitOperand = true
  }
}
