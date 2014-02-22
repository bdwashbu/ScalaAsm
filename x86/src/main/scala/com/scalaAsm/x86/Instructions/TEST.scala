package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, Opcodes, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._

class TEST extends x86Instruction("TEST")

trait TEST_2[-O1, -O2] extends TEST {
  def get(p1: O1, p2: O2): Instruction
}

object TEST {
  
  implicit object test1 extends TEST_2[r32, rm32] {
    def get(x: r32, y: rm32) = new Instruction {
      val operands = RM(x,y)
      val opcode: Opcodes = 0x85
     }
  }
  
  implicit object test2 extends TEST_2[r32, imm32] {
    def get(x: r32, y: imm32) = new Instruction {
      val operands = MI(x,y)
      val opcode: Opcodes = 0xF7 /+ 0
     }
  }
}