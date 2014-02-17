package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._

trait JNZ extends ModRM with Operands

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
  
  //implicit object jnz1 extends JNZ_1[imm8] { def get(x: imm8) = Array(0x75.toByte, x.value) }
}