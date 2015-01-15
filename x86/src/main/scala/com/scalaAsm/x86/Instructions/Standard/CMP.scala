package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CMP extends InstructionDefinition[OneOpcode]("CMP") with CMPImpl

trait CMPLow {
  implicit object CMP_59_r16_rm16 extends CMP._2_new[r16, rm16] {
    def opcode = 0x3B /r
  }

  implicit object CMP_59_r32_rm32 extends CMP._2_new[r32, rm32] {
    def opcode = 0x3B /r
    override def explicitFormat(op1: r32, op2: rm32) = {
     if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
     } else None
   }
  }

  implicit object CMP_59_r64_rm64 extends CMP._2_new[r64, rm64] {
    def opcode = 0x3B /r
    override def prefix = REX.W(true)
  }
}

trait CMPImpl extends CMPLow {
  implicit object CMP_56_rm8_r8 extends CMP._2_new[rm8, r8] {
    def opcode = 0x38 /r
  }

  implicit object CMP_57_rm16_r16 extends CMP._2_new[rm16, r16] {
    def opcode = 0x39 /r
  }

  implicit object CMP_57_rm32_r32 extends CMP._2_new[rm32, r32] {
    def opcode = 0x39 /r
    override def explicitFormat(op1: rm32, op2: r32) = {
     if (op1.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op2, op1.asInstanceOf[reg])), immediate = None))
     } else None
   }
  }

  implicit object CMP_57_rm64_r64 extends CMP._2_new[rm64, r64] {
    def opcode = 0x39 /r
    override def prefix = REX.W(true)
  }

  implicit object CMP_58_r8_rm8 extends CMP._2_new[r8, rm8] {
    def opcode = 0x3A /r
  }

  implicit object CMP_60_AL_imm8 extends CMP._2_new[AL, imm8] {
    def opcode = 0x3C
  }

  implicit object CMP_61_AX_imm16 extends CMP._2_new[AX, imm16] {
    def opcode = 0x3D
  }

  implicit object CMP_61_EAX_imm32 extends CMP._2_new[EAX, imm32] {
    def opcode = 0x3D
  }

  implicit object CMP_61_RAX_imm32 extends CMP._2_new[RAX, imm32] {
    def opcode = 0x3D
    override def prefix = REX.W(true)
  }

  implicit object CMP_128_rm8_imm8 extends CMP._2_new[rm8, imm8] {
    def opcode = 0x80 /+ 7
  }

  implicit object CMP_129_rm16_imm16 extends CMP._2_new[rm16, imm16] {
    def opcode = 0x81 /+ 7
  }

  implicit object CMP_129_rm32_imm32 extends CMP._2_new[rm32, imm32] {
    def opcode = 0x81 /+ 7
  }

  implicit object CMP_129_rm64_imm32 extends CMP._2_new[rm64, imm32] {
    def opcode = 0x81 /+ 7
    override def prefix = REX.W(true)
  }

  implicit object CMP_131_rm16_imm8 extends CMP._2_new[rm16, imm8] {
    def opcode = 0x83 /+ 7
  }

  implicit object CMP_131_rm32_imm8 extends CMP._2_new[rm32, imm8] {
    def opcode = 0x83 /+ 7
  }

  implicit object CMP_131_rm64_imm8 extends CMP._2_new[rm64, imm8] {
    def opcode = 0x83 /+ 7
    override def prefix = REX.W(true)
  }
}
