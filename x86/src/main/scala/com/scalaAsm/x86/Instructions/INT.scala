package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._

trait INT extends ModRM with Operands

trait INT_1[-O1] extends INT {
  def get(p1: O1): Instruction
}

object INT {
  
  protected[this] abstract class I[X <: Immediate](op1: X) extends Instruction1[X] {
   val opcodeExtension = None
   val operand1 = op1
   val opcode = 0xCD.toByte
   val modRM = 
      Some(new AddressingFormSpecifier {
	     val modRM = None
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = Some(op1)
	  })
  }
  
  implicit object int1 extends INT_1[imm8] {
    def get(x: imm8) = new I[imm8](x) {}
  }
}