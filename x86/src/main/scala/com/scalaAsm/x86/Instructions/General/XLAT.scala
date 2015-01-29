package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Table Look-up Translation
// Category: general/datamov

object XLAT extends InstructionDefinition[OneOpcode]("XLAT") with XLATImpl

trait XLATImpl {
  implicit object XLAT_0 extends XLAT._0 {
    def opcode = 0xD7
    override def hasImplicitOperand = true
  }
}
