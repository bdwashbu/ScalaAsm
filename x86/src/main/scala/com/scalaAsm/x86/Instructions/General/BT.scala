package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object BT extends InstructionDefinition[OneOpcode]("BT") with BTImpl

// Bit Test

trait BTImpl {
  implicit object BT_0 extends BT._2[rm16, imm8] {
    def opcode = 0xBA /+ 4
  }

  implicit object BT_1 extends BT._2[rm32, imm8] {
    def opcode = 0xBA /+ 4
  }

  implicit object BT_2 extends BT._2[rm64, imm8] {
    def opcode = 0xBA /+ 4
    override def prefix = REX.W(true)
  }
}
