package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object PUSHF extends InstructionDefinition[OneOpcode]("PUSHF") with PUSHFImpl

trait PUSHFImpl {
  implicit object PUSHF_156 extends PUSHF._0_new {
    def opcode = 0x9C
    override def hasImplicateOperand = true
  }
}
