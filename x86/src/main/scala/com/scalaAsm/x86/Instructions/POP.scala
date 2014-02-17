package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._
import scala.annotation.implicitNotFound
import com.scalaAsm.utils.Endian

trait POP extends ModRM with Operands

trait POP_1[-O1] extends POP {
  def get(p1: O1): Instruction
}



object POP {
  
  abstract class O[X <: OperandSize](op1: ModRM.reg[X]) extends Instruction1[ModRM.reg[X]] {
	  val opcodeExtension = None
	  val operand1 = op1
	  val opcode = (0x58 + op1.ID).toByte
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
      val modRM: Option[AddressingFormSpecifier] = None
     }
  }
}