package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{x86Instruction, OneOpcode, TwoOpcodes}

class ADD extends x86Instruction("ADD")

trait ADD_2[-O1, -O2] extends ADD {
  def get(op1: O1, op2: O2): Instruction
}

object ADD {
  
  implicit object add1 extends ADD_2[r32, imm8] {
    def get(x: r32, y: imm8) = new Instruction {
      val opcode = 0x83 /+ 0
      val operands = MI(x,y)
    }
  }
  
  implicit object add2 extends ADD_2[r16, imm8] {
    def get(x: r16, y: imm8) = new Instruction {
      val opcode = 0x83 /+ 0
      val operands = MI(x,y)
     }
  }
}