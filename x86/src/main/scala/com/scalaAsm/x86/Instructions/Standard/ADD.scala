package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object ADD extends InstructionDefinition[OneOpcode]("ADD") with ADDImpl

trait ADDLow {
  implicit object ADD_3_r16_rm16 extends ADD._2_new[r16, rm16] {
    def opcode = 0x3 / r
  }

  implicit object ADD_3_r32_rm32 extends ADD._2_new[r32, rm32] {
    def opcode = 0x3 / r
  }

  implicit object ADD_3_r64_rm64 extends ADD._2_new[r64, rm64] {
    def opcode = 0x3 / r
    override def prefix = REX.W(true)
  }
}

trait ADDImpl extends ADDLow {
  implicit object ADD_0_rm8_r8 extends ADD._2_new[rm8, r8] {
    def opcode = 0x0 / r
  }

  implicit object ADD_1_rm16_r16 extends ADD._2_new[rm16, r16] {
    def opcode = 0x1 / r
  }

  implicit object ADD_1_rm32_r32 extends ADD._2_new[rm32, r32] {
    def opcode = 0x1 / r
  }

  implicit object ADD_1_rm64_r64 extends ADD._2_new[rm64, r64] {
    def opcode = 0x1 / r
    override def prefix = REX.W(true)
  }

  implicit object ADD_2_r8_rm8 extends ADD._2_new[r8, rm8] {
    def opcode = 0x2 / r
  }

  implicit object ADD_4_AL_imm8 extends ADD._2_new[AL, imm8] {
    def opcode = 0x4
  }

  implicit object ADD_5_AX_imm16 extends ADD._2_new[AX, imm16] {
    def opcode = 0x5
  }

  implicit object ADD_5_EAX_imm32 extends ADD._2_new[EAX, imm32] {
    def opcode = 0x5
  }

  implicit object ADD_5_RAX_imm32 extends ADD._2_new[RAX, imm32] {
    def opcode = 0x5
  }

  implicit object ADD_128_rm8_imm8 extends ADD._2_new[rm8, imm8] {
    def opcode = 0x80 /+ 0
  }

  implicit object ADD_129_rm16_imm16 extends ADD._2_new[rm16, imm16] {
    def opcode = 0x81 /+ 0
  }

  implicit object ADD_129_rm32_imm32 extends ADD._2_new[rm32, imm32] {
    def opcode = 0x81 /+ 0
  }

  implicit object ADD_129_rm64_imm32 extends ADD._2_new[rm64, imm32] {
    def opcode = 0x81 /+ 0
    override def prefix = REX.W(true)
  }

  implicit object ADD_131_rm16_imm8 extends ADD._2_new[rm16, imm8] {
    def opcode = 0x83 /+ 0
  }

  implicit object ADD_131_rm32_imm8 extends ADD._2_new[rm32, imm8] {
    def opcode = 0x83 /+ 0
  }

  implicit object ADD_131_rm64_imm8 extends ADD._2_new[rm64, imm8] {
    def opcode = 0x83 /+ 0
    override def prefix = REX.W(true)
  }
}
