package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object SUB extends InstructionDefinition[OneOpcode]("SUB") with SUBImpl

trait SUBLow {
  implicit object SUB_43_r16_rm16 extends SUB._2_new[r16, rm16] {
    def opcode = 0x2B /r
  }

  implicit object SUB_43_r32_rm32 extends SUB._2_new[r32, rm32] {
    def opcode = 0x2B /r
  }

  implicit object SUB_43_r64_rm64 extends SUB._2_new[r64, rm64] {
    def opcode = 0x2B /r
    override def prefix = REX.W(true)
  }
}

trait SUBImpl extends SUBLow {
  implicit object SUB_40_rm8_r8 extends SUB._2_new[rm8, r8] {
    def opcode = 0x28 /r
  }

  implicit object SUB_41_rm16_r16 extends SUB._2_new[rm16, r16] {
    def opcode = 0x29 /r
  }

  implicit object SUB_41_rm32_r32 extends SUB._2_new[rm32, r32] {
    def opcode = 0x29 /r
  }

  implicit object SUB_41_rm64_r64 extends SUB._2_new[rm64, r64] {
    def opcode = 0x29 /r
    override def prefix = REX.W(true)
  }

  implicit object SUB_42_r8_rm8 extends SUB._2_new[r8, rm8] {
    def opcode = 0x2A /r
  }

  implicit object SUB_44_AL_imm8 extends SUB._2_new[AL, imm8] {
    def opcode = 0x2C
  }

  implicit object SUB_45_AX_imm16 extends SUB._2_new[AX, imm16] {
    def opcode = 0x2D
  }

  implicit object SUB_45_EAX_imm32 extends SUB._2_new[EAX, imm32] {
    def opcode = 0x2D
  }

  implicit object SUB_45_RAX_imm32 extends SUB._2_new[RAX, imm32] {
    def opcode = 0x2D
  }

  implicit object SUB_128_rm8_imm8 extends SUB._2_new[rm8, imm8] {
    def opcode = 0x80 /+ 5
  }

  implicit object SUB_129_rm16_imm16 extends SUB._2_new[rm16, imm16] {
    def opcode = 0x81 /+ 5
  }

  implicit object SUB_129_rm32_imm32 extends SUB._2_new[rm32, imm32] {
    def opcode = 0x81 /+ 5
  }

  implicit object SUB_129_rm64_imm32 extends SUB._2_new[rm64, imm32] {
    def opcode = 0x81 /+ 5
    override def prefix = REX.W(true)
  }

  implicit object SUB_131_rm16_imm8 extends SUB._2_new[rm16, imm8] {
    def opcode = 0x83 /+ 5
  }

  implicit object SUB_131_rm32_imm8 extends SUB._2_new[rm32, imm8] {
    def opcode = 0x83 /+ 5
  }

  implicit object SUB_131_rm64_imm8 extends SUB._2_new[rm64, imm8] {
    def opcode = 0x83 /+ 5
    override def prefix = REX.W(true)
  }
}
