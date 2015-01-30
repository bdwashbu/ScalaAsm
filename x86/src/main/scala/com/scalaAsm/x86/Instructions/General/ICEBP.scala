package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Call to Interrupt Procedure
// Category: general/breakstack

object ICEBP extends InstructionDefinition[OneOpcode]("ICEBP") with ICEBPImpl

trait ICEBPImpl {
  implicit object ICEBP_0 extends ICEBP._0 {
    def opcode = 0xF1
    override def hasImplicitOperand = true
  }
}
