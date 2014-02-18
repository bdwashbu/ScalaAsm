package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.AddressingFormSpecifier

trait POP extends ModRM

trait POP_1[-O1] extends POP {
  def get(p1: O1): Instruction
}



object POP {
  
  abstract class O[X <: OperandSize](op1: ModRM.reg[X]) extends Instruction1[ModRM.reg[X]] {
	  val opcodeExtension = None
	  val operand1 = op1
	  val opcode = (0x58 + op1.ID).toByte
	  val opcode2 = None
	  val modRM: Option[AddressingFormSpecifier] = None
	}

  implicit object pop1 extends POP_1[r32] {
    def get(x: r32) = new O[DwordOperand](x) {}
  }
  
  implicit object pop2 extends POP_1[DS] {
    def get(x: DS) = new Instruction1[DS] {
      val opcodeExtension = None
      val operand1 = x
      val opcode = 0x1F.toByte
      val opcode2 = None
      val modRM: Option[AddressingFormSpecifier] = None
     }
  }
}