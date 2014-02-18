package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._

trait TEST extends ModRM

trait TEST_2[-O1, -O2] extends TEST {
  def get(p1: O1, p2: O2): Instruction
}

object TEST {
  
  abstract class RM[X <: OperandSize](op1: ModRM.reg[X], op2: ModRM.rm[X]) extends Instruction2[ModRM.reg[X], ModRM.rm[X]] {
    val operand1 = op1
    val operand2 = op2
    val opcodeExtension = None
  }
  
  abstract class MI[X <: OperandSize](op1: ModRM.reg[X], op2: imm32) extends Instruction2[ModRM.reg[X], imm32] {
     val opcodeExtension = Some(0.toByte)
     val operand1 = op1
     val operand2 = op2
  }
  
  implicit object test1 extends TEST_2[r32, rm32] {
    def get(x: r32, y: rm32) = new RM[DwordOperand](x,y) {
      val opcode = 0x85.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm2(this))
     }
  }
  
  implicit object test2 extends TEST_2[r32, imm32] {
    def get(x: r32, y: imm32) = new MI[DwordOperand](x,y) {
      val opcode = 0xF7.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended2(this))
     }
  }
}