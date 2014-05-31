package com.scalaAsm.x86
package Instructions

trait JZ extends x86Instruction {
  val mnemonic = "JZ"
}

trait JZ_1[OpEn, -O1 <: Operand] extends OneOperandInstruction[OpEn, O1] with JZ

object JZ extends Formats {
  
  implicit object jz1 extends JZ_1[I, imm8] {
      val opcode = OneOpcode(0x74)
  }
}