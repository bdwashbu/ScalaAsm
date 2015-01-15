package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object NOT extends InstructionDefinition[OneOpcode]("NOT") with NOTImpl

trait NOTLow {
  implicit object NOT_247_rm16 extends NOT._1[rm16] {
    def opcode = 0xF7 /+ 2
  }

  implicit object NOT_247_rm32 extends NOT._1[rm32] {
    def opcode = 0xF7 /+ 2
  }

  implicit object NOT_247_rm64 extends NOT._1[rm64] {
    def opcode = 0xF7 /+ 2
    override def prefix = REX.W(true)
  }
}

trait NOTImpl extends NOTLow {
  implicit object NOT_246_rm8 extends NOT._1[rm8] {
    def opcode = 0xF6 /+ 2
  }
}
