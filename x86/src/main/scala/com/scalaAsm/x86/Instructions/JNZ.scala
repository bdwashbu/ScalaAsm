package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait JNZ extends ModRM

trait JNZ_1[-O1] extends JNZ {
  def get(p1: O1): Instruction
}

object JNZ {
  
  implicit object jnz1 extends JNZ_1[imm8] {
    def get(x: imm8) = new Instruction1[imm8] {
      val opcode = 0x75.toByte
      val opcodeExtension = None
      val operand1 = x
      val modRM: Option[AddressingFormSpecifier] = None
     }
  }
}