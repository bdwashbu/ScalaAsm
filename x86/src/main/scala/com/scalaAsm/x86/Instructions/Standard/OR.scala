package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object OR extends InstructionDefinition[OneOpcode]("OR") with ORImpl

trait ORLow {
  implicit object OR_11_r16_rm16 extends OR._2_new[r16, rm16] {
    def opcode = 0xB /r
  }

  implicit object OR_11_r32_rm32 extends OR._2_new[r32, rm32] {
    def opcode = 0xB /r
  }

  implicit object OR_11_r64_rm64 extends OR._2_new[r64, rm64] {
    def opcode = 0xB /r
    override def prefix = REX.W(true)
  }
}

trait ORImpl extends ORLow {
  implicit object OR_8_rm8_r8 extends OR._2_new[rm8, r8] {
    def opcode = 0x8 /r
  }

  implicit object OR_9_rm16_r16 extends OR._2_new[rm16, r16] {
    def opcode = 0x9 /r
  }

  implicit object OR_9_rm32_r32 extends OR._2_new[rm32, r32] {
    def opcode = 0x9 /r
  }

  implicit object OR_9_rm64_r64 extends OR._2_new[rm64, r64] {
    def opcode = 0x9 /r
    override def prefix = REX.W(true)
  }

  implicit object OR_10_r8_rm8 extends OR._2_new[r8, rm8] {
    def opcode = 0xA /r
  }

  implicit object OR_12_AL_imm8 extends OR._2_new[AL, imm8] {
    def opcode = 0xC
  }

  implicit object OR_13_AX_imm16 extends OR._2_new[AX, imm16] {
    def opcode = 0xD
  }

  implicit object OR_13_EAX_imm32 extends OR._2_new[EAX, imm32] {
    def opcode = 0xD
  }

  implicit object OR_13_RAX_imm32 extends OR._2_new[RAX, imm32] {
    def opcode = 0xD
  }

  implicit object OR_128_rm8_imm8 extends OR._2_new[rm8, imm8] {
    def opcode = 0x80 /+ 1
  }

  implicit object OR_129_rm16_imm16 extends OR._2_new[rm16, imm16] {
    def opcode = 0x81 /+ 1
  }

  implicit object OR_129_rm32_imm32 extends OR._2_new[rm32, imm32] {
    def opcode = 0x81 /+ 1
  }

  implicit object OR_129_rm64_imm32 extends OR._2_new[rm64, imm32] {
    def opcode = 0x81 /+ 1
    override def prefix = REX.W(true)
  }

  implicit object OR_131_rm16_imm8 extends OR._2_new[rm16, imm8] {
    def opcode = 0x83 /+ 1
  }

  implicit object OR_131_rm32_imm8 extends OR._2_new[rm32, imm8] {
    def opcode = 0x83 /+ 1
  }

  implicit object OR_131_rm64_imm8 extends OR._2_new[rm64, imm8] {
    def opcode = 0x83 /+ 1
    override def prefix = REX.W(true)
  }
}
