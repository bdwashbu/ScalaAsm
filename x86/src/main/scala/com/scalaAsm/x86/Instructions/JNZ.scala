package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait JNZ extends ModRM

trait JNZ_1[-O1] extends JNZ {
  def get(p1: O1): Instruction
}

object JNZ {
  
  implicit object jnz1 extends JNZ_1[imm8] {
    def get(x: imm8) = new Instruction {
      val operands = new OneOperand[imm8](x) {}
      val opcode = OneOpcode(0x75)
      val operand1 = x
      val modRM: Option[AddressingFormSpecifier] = None
     }
  }
}