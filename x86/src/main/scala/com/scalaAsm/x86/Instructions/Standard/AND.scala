package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object AND extends InstructionDefinition[OneOpcode]("AND") with ANDImpl

// Logical AND

trait ANDLow {
  implicit object AND_0 extends AND._2[r16, rm16] {
    def opcode = 0x23 /r
  }

  implicit object AND_1 extends AND._2[r32, rm32] {
    def opcode = 0x23 /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object AND_2 extends AND._2[r64, rm64] {
    def opcode = 0x23 /r
    override def prefix = REX.W(true)
  }
}

trait ANDImpl extends ANDLow {
  implicit object AND_3 extends AND._2[rm8, r8] {
    def opcode = 0x20 /r
  }

  implicit object AND_4 extends AND._2[rm16, r16] {
    def opcode = 0x21 /r
  }

  implicit object AND_5 extends AND._2[rm32, r32] {
    def opcode = 0x21 /r
    override def explicitFormat(op1: rm32, op2: r32) = {
      if (op1.isInstanceOf[reg]) {
         Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op2, op1.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }

  implicit object AND_6 extends AND._2[rm64, r64] {
    def opcode = 0x21 /r
    override def prefix = REX.W(true)
  }

  implicit object AND_7 extends AND._2[r8, rm8] {
    def opcode = 0x22 /r
  }

  implicit object AND_8 extends AND._2[AL, imm8] {
    def opcode = 0x24
  }

  implicit object AND_9 extends AND._2[AX, imm16] {
    def opcode = 0x25
  }

  implicit object AND_10 extends AND._2[EAX, imm32] {
    def opcode = 0x25
  }

  implicit object AND_11 extends AND._2[RAX, imm32] {
    def opcode = 0x25
    override def prefix = REX.W(true)
  }

  implicit object AND_12 extends AND._2[rm8, imm8] {
    def opcode = 0x80 /+ 4
  }

  implicit object AND_13 extends AND._2[rm16, imm16] {
    def opcode = 0x81 /+ 4
  }

  implicit object AND_14 extends AND._2[rm32, imm32] {
    def opcode = 0x81 /+ 4
  }

  implicit object AND_15 extends AND._2[rm64, imm32] {
    def opcode = 0x81 /+ 4
    override def prefix = REX.W(true)
  }
}
