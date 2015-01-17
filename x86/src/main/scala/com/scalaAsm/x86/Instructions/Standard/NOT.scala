package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object NOT extends InstructionDefinition[OneOpcode]("NOT") with NOTImpl

// One's Complement Negation

trait NOTLow {
  implicit object NOT_0 extends NOT._1[rm16] {
    def opcode = 0xF7 /+ 2
  }

  implicit object NOT_1 extends NOT._1[rm32] {
    def opcode = 0xF7 /+ 2
  }

  implicit object NOT_2 extends NOT._1[rm64] {
    def opcode = 0xF7 /+ 2
    override def prefix = REX.W(true)
  }
}

trait NOTImpl extends NOTLow {
  implicit object NOT_3 extends NOT._1[rm8] {
    def opcode = 0xF6 /+ 2
  }
}
