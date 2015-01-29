package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Shift
// Category: general/shftrot

object SHL extends InstructionDefinition[OneOpcode]("SHL") with SHLImpl

trait SHLImpl {
  implicit object SHL_0 extends SHL._2[rm8, imm8] {
    def opcode = 0xC0 /+ 4
  }

  implicit object SHL_1 extends SHL._2[rm16, imm8] {
    def opcode = 0xC1 /+ 4
  }

  implicit object SHL_2 extends SHL._2[rm32, imm8] {
    def opcode = 0xC1 /+ 4
  }

  implicit object SHL_3 extends SHL._2[rm64, imm8] {
    def opcode = 0xC1 /+ 4
    override def prefix = REX.W(true)
  }

  implicit object SHL_4 extends SHL._1[rm8] {
    def opcode = 0xD0 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object SHL_5 extends SHL._1[rm16] {
    def opcode = 0xD1 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object SHL_6 extends SHL._1[rm32] {
    def opcode = 0xD1 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object SHL_7 extends SHL._1[rm64] {
    def opcode = 0xD1 /+ 4
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}
