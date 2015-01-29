package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Table Look-up Translation
// Category: general/datamov

object XLATB extends InstructionDefinition[OneOpcode]("XLATB") with XLATBImpl

trait XLATBImpl {
  implicit object XLATB_0 extends XLATB._0 {
    def opcode = 0xD7
    override def hasImplicitOperand = true
  }
}
