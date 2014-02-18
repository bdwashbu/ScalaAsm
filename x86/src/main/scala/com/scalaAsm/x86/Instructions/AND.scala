package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction2, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait AND extends ModRM

trait AND_2[-O1, -O2] extends AND {
  def get(p1: O1, p2: O2): Instruction
}

object AND {
  
  abstract class MR[X <: OperandSize](op1: ModRM.reg[X], op2: ModRM.rm[X]) extends Instruction2[ModRM.reg[X], ModRM.rm[X]] {
	  val opcodeExtension = None
	  val operand1 = op1
	  val operand2 = op2
	  val opcode2 = None
	}
  
  implicit object and1 extends AND_2[r32, rm32] {
    def get(x: r32, y: rm32) = new MR[DwordOperand](x,y) {
      val opcode = 0x23.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm2(this))
     }
  }
}