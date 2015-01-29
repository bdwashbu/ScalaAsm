package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store String
// Category: general/datamovstring

object STOSQ extends InstructionDefinition[OneOpcode]("STOSQ") with STOSQImpl

trait STOSQImpl {
  implicit object STOSQ_0 extends STOSQ._0 {
    def opcode = 0xAB
    override def hasImplicitOperand = true
  }
}