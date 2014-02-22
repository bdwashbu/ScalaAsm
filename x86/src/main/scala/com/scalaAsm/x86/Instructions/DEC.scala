package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, OneOpcode, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait DEC

trait DEC_1[-O1] extends DEC {
  def get(p1: O1): Instruction
}

object DEC {
  
  implicit object dec1 extends DEC_1[r32] {
    def get(x: r32) = new Instruction {
      val operands = O(x)
      val opcode = OneOpcode(0x48 + x.ID)
    }
  }
  
  implicit object dec2 extends DEC_1[r16] {
    def get(x: r16) = new Instruction {
        val operands = O(x)
	    val opcode = OneOpcode(0x48 + x.ID)
     }
  }
}