package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Pop Stack into FLAGS Register
// Category: general/stackflgctrl

object POPF extends InstructionDefinition[OneOpcode]("POPF") with POPFImpl

trait POPFImpl {
  implicit object POPF_0 extends POPF._0 {
    def opcode = 0x9D
    override def hasImplicitOperand = true
  }
}
