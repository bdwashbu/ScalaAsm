package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object ADX extends InstructionDefinition[OneOpcode]("ADX") with ADXImpl

// Adjust AX Before Division

trait ADXImpl {
  implicit object ADX_0 extends ADX._1[imm8] {
    def opcode = 0xD5
    override def hasImplicitOperand = true
  }
}
