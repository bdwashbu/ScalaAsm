package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object XOR extends InstructionDefinition[OneOpcode]("XOR") with XORImpl

trait XORLow {
  implicit object XOR_51_r16_rm16 extends XOR._2_new[r16, rm16] {
    def opcode = 0x33 / r
  }

  implicit object XOR_51_r32_rm32 extends XOR._2_new[r32, rm32] {
    def opcode = 0x33 / r
  }

  implicit object XOR_51_r64_rm64 extends XOR._2_new[r64, rm64] {
    def opcode = 0x33 / r
    override def prefix = REX.W(true)
  }
}

trait XORImpl extends XORLow {
  implicit object XOR_48_rm8_r8 extends XOR._2_new[rm8, r8] {
    def opcode = 0x30 / r
  }

  implicit object XOR_49_rm16_r16 extends XOR._2_new[rm16, r16] {
    def opcode = 0x31 / r
  }

  implicit object XOR_49_rm32_r32 extends XOR._2_new[rm32, r32] {
    def opcode = 0x31 / r
  }

  implicit object XOR_49_rm64_r64 extends XOR._2_new[rm64, r64] {
    def opcode = 0x31 / r
    override def prefix = REX.W(true)
  }

  implicit object XOR_50_r8_rm8 extends XOR._2_new[r8, rm8] {
    def opcode = 0x32 / r
  }

  implicit object XOR_52_AL_imm8 extends XOR._2_new[AL, imm8] {
    def opcode = 0x34
  }

  implicit object XOR_53_AX_imm16 extends XOR._2_new[AX, imm16] {
    def opcode = 0x35
  }

  implicit object XOR_53_EAX_imm32 extends XOR._2_new[EAX, imm32] {
    def opcode = 0x35
  }

  implicit object XOR_53_RAX_imm32 extends XOR._2_new[RAX, imm32] {
    def opcode = 0x35
  }

  implicit object XOR_128_rm8_imm8 extends XOR._2_new[rm8, imm8] {
    def opcode = 0x80 /+ 6
  }

  implicit object XOR_129_rm16_imm16 extends XOR._2_new[rm16, imm16] {
    def opcode = 0x81 /+ 6
  }

  implicit object XOR_129_rm32_imm32 extends XOR._2_new[rm32, imm32] {
    def opcode = 0x81 /+ 6
  }

  implicit object XOR_129_rm64_imm32 extends XOR._2_new[rm64, imm32] {
    def opcode = 0x81 /+ 6
    override def prefix = REX.W(true)
  }

  implicit object XOR_131_rm16_imm8 extends XOR._2_new[rm16, imm8] {
    def opcode = 0x83 /+ 6
  }

  implicit object XOR_131_rm32_imm8 extends XOR._2_new[rm32, imm8] {
    def opcode = 0x83 /+ 6
  }

  implicit object XOR_131_rm64_imm8 extends XOR._2_new[rm64, imm8] {
    def opcode = 0x83 /+ 6
    override def prefix = REX.W(true)
  }
}
