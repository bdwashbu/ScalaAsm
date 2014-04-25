package com.scalaAsm.x86
package Instructions

abstract class INT extends x86Instruction("INT")

trait INT_1[-O1] extends INT with OneOperandInstruction[O1]

object INT {

  implicit object int1 extends INT_1[imm8] {
    def operands = I[imm8]()
    val opcode = OneOpcode(0xCD)
  }
}