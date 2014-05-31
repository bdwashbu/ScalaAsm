package com.scalaAsm.x86
package Instructions

trait JNZ extends x86Instruction {
  val mnemonic = "JNZ"
}

trait JNZ_1[OpEn, -O1 <: Operand] extends OneOperandInstruction[OpEn, O1] with JNZ

object JNZ extends Formats {
  
  implicit object jnz1 extends JNZ_1[I, imm8] {
      val opcode = OneOpcode(0x75)
  }
}