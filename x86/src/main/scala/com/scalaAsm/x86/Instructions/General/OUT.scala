package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object OUT extends InstructionDefinition[OneOpcode]("OUT") with OUTImpl

// Output to Port

trait OUTImpl {
  implicit object OUT_0 extends OUT._1[imm8] {
    def opcode = 0xE6
    override def hasImplicitOperand = true
  }

  implicit object OUT_1 extends OUT._0 {
    def opcode = 0xEE
    override def hasImplicitOperand = true
  }
}
