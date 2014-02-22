package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

class JNZ extends x86Instruction("JNZ")

trait JNZ_1[-O1] extends JNZ {
  def get(p1: O1): Instruction
}

object JNZ {
  
  implicit object jnz1 extends JNZ_1[imm8] {
    def get(x: imm8) = new Instruction {
      val operands = new OneOperand[imm8](x) {def getAddressingForm = null}
      val opcode = OneOpcode(0x75)
     }
  }
}