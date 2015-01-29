package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object AAS extends InstructionDefinition[OneOpcode]("AAS") with AASImpl

// ASCII Adjust AL After Subtraction

trait AASImpl {
  implicit object AAS_0 extends AAS._0 {
    def opcode = 0x3F
    override def hasImplicitOperand = true
  }
}
