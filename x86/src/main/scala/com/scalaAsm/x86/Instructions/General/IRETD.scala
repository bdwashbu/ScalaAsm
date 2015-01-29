package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Interrupt Return
// Category: general/breakstack

object IRETD extends InstructionDefinition[OneOpcode]("IRETD") with IRETDImpl

trait IRETDImpl {
  implicit object IRETD_0 extends IRETD._0 {
    def opcode = 0xCF
    override def hasImplicitOperand = true
  }
}
