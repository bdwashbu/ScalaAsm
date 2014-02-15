package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._

trait DEC 
trait DEC_1[-O1] extends DEC {
  def get(p1: O1): Instruction1
  def getBytes(op1: O1): Array[Byte]
}

trait O[X <: OperandSize] extends DEC_1[ModRM.reg[X]] {
  def getBytes(op1: ModRM.reg[X]): Array[Byte] = {
    val blah = get(op1)
    Array((blah.opcode + op1.reg.ID).toByte)
  }
}

object DEC extends ModRM with Operands {
  //implicit object dec1 extends DEC_O[r32] { def get(x: r32) = Array((0x48 + x.ID).toByte) }
 // implicit object dec2 extends DEC_O[r16] { def get(x: r16) = Array((0x48 + x.ID).toByte) }
  
  implicit object dec1 extends O[DwordOperand] {
    def get(x: r32) = {
	    Instruction.newInst1(operand1 = x, opcode = 0x48.toByte)
     }
  }
  
  implicit object dec2 extends O[WordOperand] {
    def get(x: r16) = {
	    Instruction.newInst1(operand1 = x, opcode = 0x48.toByte)
     }
  }
}