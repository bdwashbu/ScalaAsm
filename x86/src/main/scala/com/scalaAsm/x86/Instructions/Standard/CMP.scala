package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CMP extends InstructionDefinition[OneOpcode]("CMP") with CMPImpl

// Compare Two Operands

trait CMPLow {
  implicit object CMP_0 extends CMP._2[r16, rm16] {
    def opcode = 0x3B /r
  }

  implicit object CMP_1 extends CMP._2[r32, rm32] {
    def opcode = 0x3B /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object CMP_2 extends CMP._2[r64, rm64] {
    def opcode = 0x3B /r
    override def prefix = REX.W(true)
  }
}

trait CMPImpl extends CMPLow {
  implicit object CMP_3 extends CMP._2[rm8, r8] {
    def opcode = 0x38 /r
  }

  implicit object CMP_4 extends CMP._2[rm16, r16] {
    def opcode = 0x39 /r
  }

  implicit object CMP_5 extends CMP._2[rm32, r32] {
    def opcode = 0x39 /r
    override def explicitFormat(op1: rm32, op2: r32) = {
      if (op1.isInstanceOf[reg]) {
         Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op2, op1.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object CMP_6 extends CMP._2[rm64, r64] {
    def opcode = 0x39 /r
    override def prefix = REX.W(true)
  }

  implicit object CMP_7 extends CMP._2[r8, rm8] {
    def opcode = 0x3A /r
  }

  implicit object CMP_8 extends CMP._2[AL, imm8] {
    def opcode = 0x3C
  }

  implicit object CMP_9 extends CMP._2[AX, imm16] {
    def opcode = 0x3D
  }

  implicit object CMP_10 extends CMP._2[EAX, imm32] {
    def opcode = 0x3D
  }

  implicit object CMP_11 extends CMP._2[RAX, imm32] {
    def opcode = 0x3D
    override def prefix = REX.W(true)
  }

  implicit object CMP_12 extends CMP._2[rm8, imm8] {
    def opcode = 0x80 /+ 7
  }

  implicit object CMP_13 extends CMP._2[rm16, imm16] {
    def opcode = 0x81 /+ 7
  }

  implicit object CMP_14 extends CMP._2[rm32, imm32] {
    def opcode = 0x81 /+ 7
  }

  implicit object CMP_15 extends CMP._2[rm64, imm32] {
    def opcode = 0x81 /+ 7
    override def prefix = REX.W(true)
  }

  implicit object CMP_16 extends CMP._2[rm16, imm8] {
    def opcode = 0x83 /+ 7
  }

  implicit object CMP_17 extends CMP._2[rm32, imm8] {
    def opcode = 0x83 /+ 7
  }

  implicit object CMP_18 extends CMP._2[rm64, imm8] {
    def opcode = 0x83 /+ 7
    override def prefix = REX.W(true)
  }
}
