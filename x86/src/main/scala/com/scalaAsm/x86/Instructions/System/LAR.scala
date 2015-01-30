package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Load Access Rights Byte
// Category: general

object LAR extends InstructionDefinition[OneOpcode]("LAR") with LARImpl

trait LARImpl {
  implicit object LAR_0 extends LAR._2[r16, m16] {
    def opcode = 0x2 /r
  }

  implicit object LAR_1 extends LAR._2[r32, m16] {
    def opcode = 0x2 /r
  }

  implicit object LAR_2 extends LAR._2[r64, m16] {
    def opcode = 0x2 /r
    override def prefix = REX.W(true)
  }

  implicit object LAR_3 extends LAR._2[r16, r16] {
    def opcode = 0x2 /r
  }

  implicit object LAR_4 extends LAR._2[r32, r32] {
    def opcode = 0x2 /r
  }

  implicit object LAR_5 extends LAR._2[r64, r32] {
    def opcode = 0x2 /r
    override def prefix = REX.W(true)
  }
}
