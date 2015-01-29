package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object POPA extends InstructionDefinition[OneOpcode]("POPA") with POPAImpl

// Pop All General-Purpose Registers

trait POPAImpl {
  implicit object POPA_0 extends POPA._0 {
    def opcode = 0x61
    override def hasImplicitOperand = true
  }
}
