package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object SAR extends InstructionDefinition[OneOpcode]("SAR") with SARImpl

// Shift

trait SARLow {
  implicit object SAR_0 extends SAR._1[rm16] {
    def opcode = 0xD1 /+ 7
    override def hasImplicitOperand = true
  }

  implicit object SAR_1 extends SAR._1[rm32] {
    def opcode = 0xD1 /+ 7
    override def hasImplicitOperand = true
  }

  implicit object SAR_2 extends SAR._1[rm64] {
    def opcode = 0xD1 /+ 7
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}

trait SARImpl extends SARLow {
  implicit object SAR_3 extends SAR._2[rm8, imm8] {
    def opcode = 0xC0 /+ 7
  }

  implicit object SAR_4 extends SAR._2[rm16, imm8] {
    def opcode = 0xC1 /+ 7
  }

  implicit object SAR_5 extends SAR._2[rm32, imm8] {
    def opcode = 0xC1 /+ 7
  }

  implicit object SAR_6 extends SAR._2[rm64, imm8] {
    def opcode = 0xC1 /+ 7
    override def prefix = REX.W(true)
  }

  implicit object SAR_7 extends SAR._1[rm8] {
    def opcode = 0xD0 /+ 7
    override def hasImplicitOperand = true
  }
}
