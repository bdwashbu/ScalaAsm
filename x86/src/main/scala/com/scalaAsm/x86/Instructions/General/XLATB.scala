package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object XLATB extends InstructionDefinition[OneOpcode]("XLATB") with XLATBImpl

// Table Look-up Translation

trait XLATBImpl {
  implicit object XLATB_0 extends XLATB._0 {
    def opcode = 0xD7
    override def hasImplicitOperand = true
  }
}
