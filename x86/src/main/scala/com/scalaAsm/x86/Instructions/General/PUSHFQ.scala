package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Push rFLAGS Register onto the Stack
// Category: general/stackflgctrl

object PUSHFQ extends InstructionDefinition[OneOpcode]("PUSHFQ") with PUSHFQImpl

trait PUSHFQImpl {
  implicit object PUSHFQ_0 extends PUSHFQ._0 {
    def opcode = 0x9C
    override def hasImplicitOperand = true
  }
}
