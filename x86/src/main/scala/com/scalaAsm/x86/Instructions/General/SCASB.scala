package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Scan String
// Category: general/arithstring/binary

object SCASB extends InstructionDefinition[OneOpcode]("SCASB") with SCASBImpl

trait SCASBImpl {
  implicit object SCASB_0 extends SCASB._0 {
    def opcode = 0xAE
    override def hasImplicitOperand = true
  }
}