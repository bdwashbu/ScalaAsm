package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: One's Complement Negation
// Category: general/logical

object NOT extends InstructionDefinition[OneOpcode]("NOT") with NOTImpl

trait NOTImpl {
  implicit object NOT_0 extends NOT._1[rm8] {
    def opcode = 0xF6 /+ 2
  }

  implicit object NOT_1 extends NOT._1[rm16] {
    def opcode = 0xF7 /+ 2
  }

  implicit object NOT_2 extends NOT._1[rm32] {
    def opcode = 0xF7 /+ 2
  }

  implicit object NOT_3 extends NOT._1[rm64] {
    def opcode = 0xF7 /+ 2
    override def prefix = REX.W(true)
  }
}
