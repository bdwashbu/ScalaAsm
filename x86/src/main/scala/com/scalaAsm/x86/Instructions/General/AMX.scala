package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object AMX extends InstructionDefinition[OneOpcode]("AMX") with AMXImpl

// Adjust AX After Multiply

trait AMXImpl {
  implicit object AMX_0 extends AMX._1[imm8] {
    def opcode = 0xD4
    override def hasImplicitOperand = true
  }
}
