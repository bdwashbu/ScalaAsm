package com.scalaAsm.x86
package Instructions
package Standard

trait INT extends x86Instruction {
  val mnemonic = "INT"
}

trait INT_1[OpEn, -O1 <: Operand] extends OneOperandInstruction[OpEn, O1] with INT

object INT {

  implicit object int1 extends INT_1[I, imm8] {
    val opcode = OneOpcode(0xCD)
  }
}