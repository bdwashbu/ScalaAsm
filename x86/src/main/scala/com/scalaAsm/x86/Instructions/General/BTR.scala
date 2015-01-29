package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Bit Test and Reset
// Category: general/bit

object BTR extends InstructionDefinition[OneOpcode]("BTR") with BTRImpl

trait BTRImpl {
  implicit object BTR_0 extends BTR._2[rm16, imm8] {
    def opcode = 0xBA /+ 6
  }

  implicit object BTR_1 extends BTR._2[rm32, imm8] {
    def opcode = 0xBA /+ 6
  }

  implicit object BTR_2 extends BTR._2[rm64, imm8] {
    def opcode = 0xBA /+ 6
    override def prefix = REX.W(true)
  }
}
