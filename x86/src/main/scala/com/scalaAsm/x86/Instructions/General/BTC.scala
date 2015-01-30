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

  implicit object BTC_3 extends BTC._2[rm16, r16] {
    def opcode = 0xBB /r
  }

  implicit object BTC_4 extends BTC._2[rm32, r32] {
    def opcode = 0xBB /r
    override def explicitFormat(op1: rm32, op2: r32) = {
      if (op1.isInstanceOf[reg]) {
         Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op2, op1.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object BTC_5 extends BTC._2[rm64, r64] {
    def opcode = 0xBB /r
    override def prefix = REX.W(true)
  }
}
