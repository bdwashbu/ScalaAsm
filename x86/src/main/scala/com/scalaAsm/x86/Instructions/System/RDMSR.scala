package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Read from Model Specific Register
// Category: general

object RDMSR extends InstructionDefinition[OneOpcode]("RDMSR") with RDMSRImpl

trait RDMSRImpl {
  implicit object RDMSR_0 extends RDMSR._0 {
    def opcode = 0x32
    override def hasImplicitOperand = true
  }
}
