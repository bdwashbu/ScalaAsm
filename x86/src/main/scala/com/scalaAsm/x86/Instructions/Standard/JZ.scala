package com.scalaAsm.x86
package Instructions
package Standard

trait JZ extends x86Instruction {
  val mnemonic = "JZ"
}

trait JZ_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode] with JZ

object JZ {
  
  implicit object jz1 extends JZ_1[imm8, I] {
      val opcode = OneOpcode(0x74)
  }
}