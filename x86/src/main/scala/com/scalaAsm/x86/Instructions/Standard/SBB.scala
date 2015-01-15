package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object SBB extends InstructionDefinition[OneOpcode]("SBB") with SBBImpl

trait SBBLow {
  implicit object SBB_27_r16_rm16 extends SBB._2[r16, rm16] {
    def opcode = 0x1B /r
  }

  implicit object SBB_27_r32_rm32 extends SBB._2[r32, rm32] {
    def opcode = 0x1B /r
  }

  implicit object SBB_27_r64_rm64 extends SBB._2[r64, rm64] {
    def opcode = 0x1B /r
    override def prefix = REX.W(true)
  }
}

trait SBBImpl extends SBBLow {
  implicit object SBB_24_rm8_r8 extends SBB._2[rm8, r8] {
    def opcode = 0x18 /r
  }

  implicit object SBB_25_rm16_r16 extends SBB._2[rm16, r16] {
    def opcode = 0x19 /r
  }

  implicit object SBB_25_rm32_r32 extends SBB._2[rm32, r32] {
    def opcode = 0x19 /r
  }

  implicit object SBB_25_rm64_r64 extends SBB._2[rm64, r64] {
    def opcode = 0x19 /r
    override def prefix = REX.W(true)
  }

  implicit object SBB_26_r8_rm8 extends SBB._2[r8, rm8] {
    def opcode = 0x1A /r
  }

  implicit object SBB_28_AL_imm8 extends SBB._2[AL, imm8] {
    def opcode = 0x1C
  }

  implicit object SBB_29_AX_imm16 extends SBB._2[AX, imm16] {
    def opcode = 0x1D
  }

  implicit object SBB_29_EAX_imm32 extends SBB._2[EAX, imm32] {
    def opcode = 0x1D
  }

  implicit object SBB_29_RAX_imm32 extends SBB._2[RAX, imm32] {
    def opcode = 0x1D
  }

  implicit object SBB_128_rm8_imm8 extends SBB._2[rm8, imm8] {
    def opcode = 0x80 /+ 3
  }

  implicit object SBB_129_rm16_imm16 extends SBB._2[rm16, imm16] {
    def opcode = 0x81 /+ 3
  }

  implicit object SBB_129_rm32_imm32 extends SBB._2[rm32, imm32] {
    def opcode = 0x81 /+ 3
  }

  implicit object SBB_129_rm64_imm32 extends SBB._2[rm64, imm32] {
    def opcode = 0x81 /+ 3
    override def prefix = REX.W(true)
  }

  implicit object SBB_131_rm16_imm8 extends SBB._2[rm16, imm8] {
    def opcode = 0x83 /+ 3
  }

  implicit object SBB_131_rm32_imm8 extends SBB._2[rm32, imm8] {
    def opcode = 0x83 /+ 3
  }

  implicit object SBB_131_rm64_imm8 extends SBB._2[rm64, imm8] {
    def opcode = 0x83 /+ 3
    override def prefix = REX.W(true)
  }
}
