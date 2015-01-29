package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Rotate
// Category: general/shftrot

object RCR extends InstructionDefinition[OneOpcode]("RCR") with RCRImpl

trait RCRImpl {
  implicit object RCR_0 extends RCR._2[rm8, imm8] {
    def opcode = 0xC0 /+ 3
  }

  implicit object RCR_1 extends RCR._2[rm16, imm8] {
    def opcode = 0xC1 /+ 3
  }

  implicit object RCR_2 extends RCR._2[rm32, imm8] {
    def opcode = 0xC1 /+ 3
  }

  implicit object RCR_3 extends RCR._2[rm64, imm8] {
    def opcode = 0xC1 /+ 3
    override def prefix = REX.W(true)
  }

  implicit object RCR_4 extends RCR._1[rm8] {
    def opcode = 0xD0 /+ 3
    override def hasImplicitOperand = true
  }

  implicit object RCR_5 extends RCR._1[rm16] {
    def opcode = 0xD1 /+ 3
    override def hasImplicitOperand = true
  }

  implicit object RCR_6 extends RCR._1[rm32] {
    def opcode = 0xD1 /+ 3
    override def hasImplicitOperand = true
  }

  implicit object RCR_7 extends RCR._1[rm64] {
    def opcode = 0xD1 /+ 3
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}
