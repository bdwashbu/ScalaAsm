package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._

trait DEC
trait DEC_1[-O1] extends DEC {
  def get(p1: O1): Instruction
}

trait O[X <: OperandSize] extends Instruction1[ModRM.reg[X]] {
   var opcodeExtension = 0.toByte
}

object DEC {

  implicit object dec1 extends DEC_1[r32] {
    def get(x: r32) = new O[DwordOperand] {
      var opcode = (0x48 + x.ID).toByte
      var operand1 = x
      var modRM: AddressingFormSpecifier = Instruction.newInst1(this)
    }
  }
  
  implicit object dec2 extends DEC_1[r16] {
    def get(x: r16) = new O[WordOperand] {
	    var opcode = (0x48 + x.ID).toByte
	    var operand1 = x
	    var modRM: AddressingFormSpecifier = Instruction.newInst1(this)
     }
  }
}