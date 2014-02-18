package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait DEC

trait DEC_1[-O1] extends DEC {
  def get(p1: O1): Instruction
}

object DEC {

  protected[this] abstract class O[X <: OperandSize](op1: ModRM.reg[X]) extends Instruction1[ModRM.reg[X]] {
   val opcodeExtension = None
   val operand1 = op1
   val opcode2 = None
  }
  
  implicit object dec1 extends DEC_1[r32] {
    def get(x: r32) = new O[DwordOperand](x) {
      val opcode = (0x48 + x.ID).toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended1(this))
    }
  }
  
  implicit object dec2 extends DEC_1[r16] {
    def get(x: r16) = new O[WordOperand](x) {
	    val opcode = (0x48 + x.ID).toByte
	    val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended1(this))
     }
  }
}