package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object SHR extends InstructionDefinition[OneOpcode]("SHR") with SHRImpl

// Shift

trait SHRLow {
  implicit object SHR_0 extends SHR._1[rm16] {
    def opcode = 0xD1 /+ 5
    override def hasImplicateOperand = true
  }

  implicit object SHR_1 extends SHR._1[rm32] {
    def opcode = 0xD1 /+ 5
    override def hasImplicateOperand = true
  }

  implicit object SHR_2 extends SHR._1[rm64] {
    def opcode = 0xD1 /+ 5
    override def prefix = REX.W(true)
    override def hasImplicateOperand = true
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
    override def hasImplicateOperand = true
  }

  implicit object SHR_8 extends SHR._2[rm8, CL] {
    def opcode = 0xD2 /+ 5
  }

  implicit object SHR_9 extends SHR._2[rm16, CL] {
    def opcode = 0xD3 /+ 5
  }

  implicit object SHR_10 extends SHR._2[rm32, CL] {
    def opcode = 0xD3 /+ 5
  }

  implicit object SHR_11 extends SHR._2[rm64, CL] {
    def opcode = 0xD3 /+ 5
    override def prefix = REX.W(true)
  }
}
