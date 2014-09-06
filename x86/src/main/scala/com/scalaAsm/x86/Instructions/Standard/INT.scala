package com.scalaAsm.x86
package Instructions
package Standard

trait INT extends x86Instruction {
  val mnemonic = "INT"
}

trait INT_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode] with INT

object INT {

  implicit object int1 extends INT_1[imm8, I] {
    val opcode = OneOpcode(0xCD)
  }
}