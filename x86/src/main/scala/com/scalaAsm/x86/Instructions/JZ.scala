package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait JZ extends ModRM

trait JZ_1[-O1] extends JZ {
  def get(p1: O1): Instruction
}

object JZ {
  
  implicit object jz1 extends JZ_1[imm8] {
    def get(x: imm8) = new Instruction1[imm8] {
      val opcode = 0x74.toByte
      val opcodeExtension = None
      val operand1 = x
      val opcode2 = None
      val modRM = 
        Some(new AddressingFormSpecifier {
	        val modRM = None
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = Some(x)
	     })
     }
  }
}