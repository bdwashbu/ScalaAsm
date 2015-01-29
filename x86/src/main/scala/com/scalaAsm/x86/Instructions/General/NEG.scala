package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Two's Complement Negation
// Category: general/arith/binary

object NEG extends InstructionDefinition[OneOpcode]("NEG") with NEGImpl

trait NEGImpl {
  implicit object NEG_0 extends NEG._1[rm8] {
    def opcode = 0xF6 /+ 3
  }

  implicit object NEG_1 extends NEG._1[rm16] {
    def opcode = 0xF7 /+ 3
  }

  implicit object NEG_2 extends NEG._1[rm32] {
    def opcode = 0xF7 /+ 3
  }

  implicit object NEG_3 extends NEG._1[rm64] {
    def opcode = 0xF7 /+ 3
    override def prefix = REX.W(true)
  }
}
