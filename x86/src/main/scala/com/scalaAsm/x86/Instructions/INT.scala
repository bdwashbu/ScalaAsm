package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait INT

trait INT_1[-O1] extends INT {
  def get(p1: O1): Instruction
}

object INT {

  implicit object int1 extends INT_1[imm8] {
    def get(x: imm8) = new Instruction {
      val operands = I[imm8](x)
      val opcode = OneOpcode(0xCD)
    }
  }
}