package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object SHR extends InstructionDefinition[OneOpcode]("SHR") with SHRImpl

// Shift

trait SHRLow {
  implicit object SHR_0 extends SHR._1[rm16] {
    def opcode = 0xD1 /+ 5
    override def hasImplicitOperand = true
  }

  implicit object SHR_1 extends SHR._1[rm32] {
    def opcode = 0xD1 /+ 5
    override def hasImplicitOperand = true
  }

  implicit object SHR_2 extends SHR._1[rm64] {
    def opcode = 0xD1 /+ 5
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}

trait SHRImpl extends SHRLow {
  implicit object SHR_3 extends SHR._2[rm8, imm8] {
    def opcode = 0xC0 /+ 5
  }

  implicit object SHR_4 extends SHR._2[rm16, imm8] {
    def opcode = 0xC1 /+ 5
  }

  implicit object SHR_5 extends SHR._2[rm32, imm8] {
    def opcode = 0xC1 /+ 5
  }

  implicit object SHR_6 extends SHR._2[rm64, imm8] {
    def opcode = 0xC1 /+ 5
    override def prefix = REX.W(true)
  }

  implicit object SHR_7 extends SHR._1[rm8] {
    def opcode = 0xD0 /+ 5
    override def hasImplicitOperand = true
  }
}
