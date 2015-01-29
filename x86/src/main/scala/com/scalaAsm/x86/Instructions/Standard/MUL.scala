package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object MUL extends InstructionDefinition[OneOpcode]("MUL") with MULImpl

// Unsigned Multiply

trait MULLow {
  implicit object MUL_0 extends MUL._1[rm16] {
    def opcode = 0xF7 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object MUL_1 extends MUL._1[rm32] {
    def opcode = 0xF7 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object MUL_2 extends MUL._1[rm64] {
    def opcode = 0xF7 /+ 4
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}

trait MULImpl extends MULLow {
  implicit object MUL_3 extends MUL._1[rm8] {
    def opcode = 0xF6 /+ 4
    override def hasImplicitOperand = true
  }
}
