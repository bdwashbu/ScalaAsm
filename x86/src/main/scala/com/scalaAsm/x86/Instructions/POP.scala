package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.AddressingFormSpecifier

class POP extends x86Instruction("POP ")

trait POP_1[-O1] extends POP {
  def get(p1: O1): Instruction
}

object POP {
  
  implicit object pop1 extends POP_1[r32] {
    def get(x: r32) = new Instruction {
      val operands = O(x)
      val opcode = OneOpcode(0x58 + x.ID)
    }
  }
  
  implicit object pop2 extends POP_1[DS] {
    def get(x: DS) = new Instruction {
      val operands = new OneOperand[DS](x) {def getAddressingForm = null}
      val opcode = OneOpcode(0x1F)
     }
  }
}