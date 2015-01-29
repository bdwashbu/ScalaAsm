package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object SAL extends InstructionDefinition[OneOpcode]("SAL") with SALImpl

// Shift

trait SALLow {
  implicit object SAL_0 extends SAL._1[rm16] {
    def opcode = 0xD1 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object SAL_1 extends SAL._1[rm32] {
    def opcode = 0xD1 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object SAL_2 extends SAL._1[rm64] {
    def opcode = 0xD1 /+ 4
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}

trait SALImpl extends SALLow {
  implicit object SAL_3 extends SAL._2[rm8, imm8] {
    def opcode = 0xC0 /+ 4
  }

  implicit object SAL_4 extends SAL._2[rm16, imm8] {
    def opcode = 0xC1 /+ 4
  }

  implicit object SAL_5 extends SAL._2[rm32, imm8] {
    def opcode = 0xC1 /+ 4
  }

  implicit object SAL_6 extends SAL._2[rm64, imm8] {
    def opcode = 0xC1 /+ 4
    override def prefix = REX.W(true)
  }

  implicit object SAL_7 extends SAL._1[rm8] {
    def opcode = 0xD0 /+ 4
    override def hasImplicitOperand = true
  }
}
