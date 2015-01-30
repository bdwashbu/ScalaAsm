package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Resume from System Management Mode
// Category: general/branch

object RSM extends InstructionDefinition[OneOpcode]("RSM") with RSMImpl

trait RSMImpl {
  implicit object RSM_0 extends RSM._0 {
    def opcode = 0xAA
    override def hasImplicitOperand = true
  }
}
