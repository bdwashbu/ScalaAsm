package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object MOVSB extends InstructionDefinition[OneOpcode]("MOVSB") with MOVSBImpl

// Move Data from String to String

trait MOVSBImpl {
  implicit object MOVSB_0 extends MOVSB._0 {
    def opcode = 0xA4
    override def hasImplicitOperand = true
  }
}
