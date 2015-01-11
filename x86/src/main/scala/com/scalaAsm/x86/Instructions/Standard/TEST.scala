package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object TEST extends InstructionDefinition[OneOpcode]("TEST") with TESTImpl

trait TESTImpl {
  implicit object TEST_132_rm8_r8 extends TEST._2_new[rm8, r8] {
    def opcode = 0x84 /r
  }

  implicit object TEST_133_rm16_r16 extends TEST._2_new[rm16, r16] {
    def opcode = 0x85 /r
  }

  implicit object TEST_133_rm32_r32 extends TEST._2_new[rm32, r32] {
    def opcode = 0x85 /r
  }

  implicit object TEST_133_rm64_r64 extends TEST._2_new[rm64, r64] {
    def opcode = 0x85 /r
    override def prefix = REX.W(true)
  }

  implicit object TEST_168_AL_imm8 extends TEST._2_new[AL, imm8] {
    def opcode = 0xA8
  }

  implicit object TEST_169_AX_imm16 extends TEST._2_new[AX, imm16] {
    def opcode = 0xA9
  }

  implicit object TEST_169_EAX_imm32 extends TEST._2_new[EAX, imm32] {
    def opcode = 0xA9
  }

  implicit object TEST_169_RAX_imm32 extends TEST._2_new[RAX, imm32] {
    def opcode = 0xA9
    override def prefix = REX.W(true)
  }

  implicit object TEST_246_rm8_imm8 extends TEST._2_new[rm8, imm8] {
    def opcode = 0xF6 /+ 0
  }

  implicit object TEST_247_rm16_imm16 extends TEST._2_new[rm16, imm16] {
    def opcode = 0xF7 /+ 0
  }

  implicit object TEST_247_rm32_imm32 extends TEST._2_new[rm32, imm32] {
    def opcode = 0xF7 /+ 0
  }

  implicit object TEST_247_rm64_imm64 extends TEST._2_new[rm64, imm64] {
    def opcode = 0xF7 /+ 0
    override def prefix = REX.W(true)
  }
}
