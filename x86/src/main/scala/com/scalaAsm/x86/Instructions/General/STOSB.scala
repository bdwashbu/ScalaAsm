package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object STOSB extends InstructionDefinition[OneOpcode]("STOSB") with STOSBImpl

// Store String

trait STOSBImpl {
  implicit object STOSB_0 extends STOSB._0 {
    def opcode = 0xAA
    override def hasImplicitOperand = true
  }
}
