package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object INSB extends InstructionDefinition[OneOpcode]("INSB") with INSBImpl

// Input from Port to String

trait INSBImpl {
  implicit object INSB_0 extends INSB._0 {
    def opcode = 0x6C
    override def hasImplicitOperand = true
  }
}
