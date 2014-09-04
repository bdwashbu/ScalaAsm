package com.scalaAsm.x86
package Instructions
package Standard

trait JNZ extends x86Instruction {
  val mnemonic = "JNZ"
}

trait JNZ_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn] with JNZ

object JNZ {
  
  implicit object jnz1 extends JNZ_1[imm8, I] {
      val opcode = OneOpcode(0x75)
  }
}