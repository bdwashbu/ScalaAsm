package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait INT extends ModRM

trait INT_1[-O1] extends INT {
  def get(p1: O1): Instruction
}

object INT {
  
  protected[this] abstract class I[X <: Immediate](op1: X) extends Instruction1[X] {
   val opcodeExtension = None
   val operand1 = op1
   val opcode = 0xCD.toByte
   val opcode2 = None
  }
  
  implicit object int1 extends INT_1[imm8] {
    def get(x: imm8) = new I[imm8](x) {
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm1(this))
    }
  }
}