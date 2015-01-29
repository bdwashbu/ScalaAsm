package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Return from procedure
// Category: general/branchstack

object RETN extends InstructionDefinition[OneOpcode]("RETN") with RETNImpl

trait RETNImpl {
  implicit object RETN_0 extends RETN._1[imm16] {
    def opcode = 0xC2
    override def hasImplicitOperand = true
  }

  implicit object RETN_1 extends RETN._0 {
    def opcode = 0xC3
    override def hasImplicitOperand = true
  }
}
