package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object STOSQ extends InstructionDefinition[OneOpcode]("STOSQ") with STOSQImpl

// Store String

trait STOSQImpl {
  implicit object STOSQ_0 extends STOSQ._0 {
    def opcode = 0xAB
    override def hasImplicitOperand = true
  }
}
