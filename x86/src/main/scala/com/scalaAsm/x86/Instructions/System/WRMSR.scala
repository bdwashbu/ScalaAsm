package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Write to Model Specific Register
// Category: general

object WRMSR extends InstructionDefinition[OneOpcode]("WRMSR") with WRMSRImpl

trait WRMSRImpl {
  implicit object WRMSR_0 extends WRMSR._0 {
    def opcode = 0x30
    override def hasImplicitOperand = true
  }
}
