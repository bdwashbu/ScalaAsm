package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Output String to Port
// Category: general/inoutstring

object OUTSD extends InstructionDefinition[OneOpcode]("OUTSD") with OUTSDImpl

trait OUTSDImpl {
  implicit object OUTSD_0 extends OUTSD._0 {
    def opcode = 0x6F
    override def hasImplicitOperand = true
  }
}
