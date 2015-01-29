package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Output String to Port
// Category: general/inoutstring

object OUTS extends InstructionDefinition[OneOpcode]("OUTS") with OUTSImpl

trait OUTSImpl {
  implicit object OUTS_0 extends OUTS._0 {
    def opcode = 0x6E
    override def hasImplicitOperand = true
  }
}
