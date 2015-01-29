package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object RETF extends InstructionDefinition[OneOpcode]("RETF") with RETFImpl

// Return from procedure

trait RETFImpl {
  implicit object RETF_0 extends RETF._1[imm16] {
    def opcode = 0xCA
    override def hasImplicitOperand = true
  }

  implicit object RETF_1 extends RETF._0 {
    def opcode = 0xCB
    override def hasImplicitOperand = true
  }
}
