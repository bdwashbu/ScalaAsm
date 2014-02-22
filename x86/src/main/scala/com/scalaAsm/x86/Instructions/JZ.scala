package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Immediate, OneOpcode, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait JZ extends ModRM

trait JZ_1[-O1] extends JZ {
  def get(p1: O1): Instruction
}

object JZ {
  
  implicit object jz1 extends JZ_1[imm8] {
    def get(x: imm8) = new Instruction {
      val operands = new OneOperand[imm8](x) {}
      val opcode = OneOpcode(0x74)
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