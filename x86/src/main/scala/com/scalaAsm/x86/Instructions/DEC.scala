package com.scalaAsm.x86
package Instructions

trait DEC extends x86Instruction {
  val mnemonic = "DEC"
}

trait DEC_1[OpEn, -O1 <: Operand] extends OneOperandInstruction[OpEn, O1] with DEC

object DEC {
  
  implicit object dec1 extends DEC_1[O, r32] {
     val opcode = OneOpcode(0x48) + rd
  }
  
  implicit object dec2 extends DEC_1[O, r16] {
	 val opcode = OneOpcode(0x48) + rw
  }
}