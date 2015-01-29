package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object OUTS extends InstructionDefinition[OneOpcode]("OUTS") with OUTSImpl

// Output String to Port

trait OUTSImpl {
  implicit object OUTS_0 extends OUTS._0 {
    def opcode = 0x6E
    override def hasImplicitOperand = true
  }
}
