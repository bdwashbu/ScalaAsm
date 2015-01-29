package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Push FLAGS Register onto the Stack
// Category: general/stackflgctrl

object PUSHF extends InstructionDefinition[OneOpcode]("PUSHF") with PUSHFImpl

trait PUSHFImpl {
  implicit object PUSHF_0 extends PUSHF._0 {
    def opcode = 0x9C
    override def hasImplicitOperand = true
  }
}
