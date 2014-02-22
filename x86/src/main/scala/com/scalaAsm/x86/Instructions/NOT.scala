package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

class NOT extends x86Instruction("NOT")

trait NOT_1[-O1] extends NOT {
  def get(p: O1): Instruction
}

object NOT {
  
  implicit object not1 extends NOT_1[rm32] {
    def get(x: rm32) = new Instruction {
      val operands = M(x)
      val opcode = 0xF7 /+ 2
     }
  }
}