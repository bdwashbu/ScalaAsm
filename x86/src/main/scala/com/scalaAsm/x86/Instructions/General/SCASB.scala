package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object SCASB extends InstructionDefinition[OneOpcode]("SCASB") with SCASBImpl

// Scan String

trait SCASBImpl {
  implicit object SCASB_0 extends SCASB._0 {
    def opcode = 0xAE
    override def hasImplicitOperand = true
  }
}
