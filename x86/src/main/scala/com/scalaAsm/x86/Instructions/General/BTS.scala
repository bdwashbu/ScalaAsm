package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object BTS extends InstructionDefinition[OneOpcode]("BTS") with BTSImpl

// Bit Test and Set

trait BTSImpl {
  implicit object BTS_0 extends BTS._2[rm16, imm8] {
    def opcode = 0xBA /+ 5
  }

  implicit object BTS_1 extends BTS._2[rm32, imm8] {
    def opcode = 0xBA /+ 5
  }

  implicit object BTS_2 extends BTS._2[rm64, imm8] {
    def opcode = 0xBA /+ 5
    override def prefix = REX.W(true)
  }
}
