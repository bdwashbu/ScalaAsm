package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Move Data from String to String
// Category: general/datamovstring

object MOVSD extends InstructionDefinition[OneOpcode]("MOVSD") with MOVSDImpl

trait MOVSDImpl {
  implicit object MOVSD_0 extends MOVSD._0 {
    def opcode = 0xA5
    override def hasImplicitOperand = true
  }
}
