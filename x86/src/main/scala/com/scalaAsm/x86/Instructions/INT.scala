package com.scalaAsm.x86
package Instructions

trait INT extends x86Instruction {
  val mnemonic = "INT"
}

trait INT_1[-O1 <: Operand] extends INT with OneOperandInstruction[O1]

object INT {

  implicit object int1 extends INT_1[imm8] {
    def opEn = I
    val opcode = OneOpcode(0xCD)
  }
}