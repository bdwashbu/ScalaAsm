package com.scalaAsm.x86
package Instructions

abstract class INT extends x86Instruction("INT")

trait INT_1[-O1] extends INT with OneOperand[O1] with OperandEncoding

object INT {

  implicit object int1 extends INT_1[imm8] {
    def operands = I[imm8](x)
    val opcode = OneOpcode(0xCD)
  }
}