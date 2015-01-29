package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Bit Test and Complement
// Category: general/bit

object BTC extends InstructionDefinition[OneOpcode]("BTC") with BTCImpl

trait BTCImpl {
  implicit object BTC_0 extends BTC._2[rm16, imm8] {
    def opcode = 0xBA /+ 7
  }

  implicit object BTC_1 extends BTC._2[rm32, imm8] {
    def opcode = 0xBA /+ 7
  }

  implicit object BTC_2 extends BTC._2[rm64, imm8] {
    def opcode = 0xBA /+ 7
    override def prefix = REX.W(true)
  }
}
