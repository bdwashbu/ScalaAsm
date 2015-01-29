package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store String
// Category: general/datamovstring

object STOSD extends InstructionDefinition[OneOpcode]("STOSD") with STOSDImpl

trait STOSDImpl {
  implicit object STOSD_0 extends STOSD._0 {
    def opcode = 0xAB
    override def hasImplicitOperand = true
  }
}
