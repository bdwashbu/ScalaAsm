package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object AND extends InstructionDefinition[OneOpcode]("AND") with ANDImpl

trait ANDLow {
  implicit object AND_35_r16_rm16 extends AND._2_new[r16, rm16] {
    def opcode = 0x23 /r
  }

  implicit object AND_35_r32_rm32 extends AND._2_new[r32, rm32] {
    def opcode = 0x23 /r
    override def explicitFormat(op1: r32, op2: rm32) = {
     if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
     } else None
   }
  }

  implicit object AND_35_r64_rm64 extends AND._2_new[r64, rm64] {
    def opcode = 0x23 /r
    override def prefix = REX.W(true)
  }
}

trait ANDImpl extends ANDLow {
  implicit object AND_32_rm8_r8 extends AND._2_new[rm8, r8] {
    def opcode = 0x20 /r
  }

  implicit object AND_33_rm16_r16 extends AND._2_new[rm16, r16] {
    def opcode = 0x21 /r
  }

  implicit object AND_33_rm32_r32 extends AND._2_new[rm32, r32] {
    def opcode = 0x21 /r
    override def explicitFormat(op1: rm32, op2: r32) = {
     if (op1.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op2, op1.asInstanceOf[reg])), immediate = None))
     } else None
   }
  }

  implicit object AND_33_rm64_r64 extends AND._2_new[rm64, r64] {
    def opcode = 0x21 /r
    override def prefix = REX.W(true)
  }

  implicit object AND_34_r8_rm8 extends AND._2_new[r8, rm8] {
    def opcode = 0x22 /r
  }

  implicit object AND_36_AL_imm8 extends AND._2_new[AL, imm8] {
    def opcode = 0x24
  }

  implicit object AND_37_AX_imm16 extends AND._2_new[AX, imm16] {
    def opcode = 0x25
  }

  implicit object AND_37_EAX_imm32 extends AND._2_new[EAX, imm32] {
    def opcode = 0x25
  }

  implicit object AND_37_RAX_imm32 extends AND._2_new[RAX, imm32] {
    def opcode = 0x25
    override def prefix = REX.W(true)
  }

  implicit object AND_128_rm8_imm8 extends AND._2_new[rm8, imm8] {
    def opcode = 0x80 /+ 4
  }

  implicit object AND_129_rm16_imm16 extends AND._2_new[rm16, imm16] {
    def opcode = 0x81 /+ 4
  }

  implicit object AND_129_rm32_imm32 extends AND._2_new[rm32, imm32] {
    def opcode = 0x81 /+ 4
  }

  implicit object AND_129_rm64_imm32 extends AND._2_new[rm64, imm32] {
    def opcode = 0x81 /+ 4
    override def prefix = REX.W(true)
  }
}
