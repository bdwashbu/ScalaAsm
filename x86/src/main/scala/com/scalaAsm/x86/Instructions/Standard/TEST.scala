package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object TEST extends InstructionDefinition[OneOpcode]("TEST") with TESTImpl

// Logical Compare

trait TESTImpl {
  implicit object TEST_0 extends TEST._2[rm8, r8] {
    def opcode = 0x84 /r
  }

  implicit object TEST_1 extends TEST._2[rm16, r16] {
    def opcode = 0x85 /r
  }

  implicit object TEST_2 extends TEST._2[rm32, r32] {
    def opcode = 0x85 /r
    override def explicitFormat(op1: rm32, op2: r32) = {
      if (op1.isInstanceOf[reg]) {
         Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op2, op1.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object TEST_3 extends TEST._2[rm64, r64] {
    def opcode = 0x85 /r
    override def prefix = REX.W(true)
  }

  implicit object TEST_4 extends TEST._2[AL, imm8] {
    def opcode = 0xA8
  }

  implicit object TEST_5 extends TEST._2[AX, imm16] {
    def opcode = 0xA9
  }

  implicit object TEST_6 extends TEST._2[EAX, imm32] {
    def opcode = 0xA9
  }

  implicit object TEST_7 extends TEST._2[RAX, imm32] {
    def opcode = 0xA9
    override def prefix = REX.W(true)
  }

  implicit object TEST_8 extends TEST._2[rm8, imm8] {
    def opcode = 0xF6 /+ 0
  }

  implicit object TEST_9 extends TEST._2[rm16, imm16] {
    def opcode = 0xF7 /+ 0
  }

  implicit object TEST_10 extends TEST._2[rm32, imm32] {
    def opcode = 0xF7 /+ 0
  }

  implicit object TEST_11 extends TEST._2[rm64, imm64] {
    def opcode = 0xF7 /+ 0
    override def prefix = REX.W(true)
  }
}
