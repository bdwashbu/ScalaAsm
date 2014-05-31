package com.scalaAsm.x86
package Instructions

trait DEC extends x86Instruction {
  val mnemonic = "DEC"
}

trait DEC_1[OpEn, -O1 <: Operand] extends OneOperandInstruction[OpEn, O1] with DEC

object DEC extends Formats {
  
  implicit object dec1 extends DEC_1[O, r32] {
     val opcode = OpcodePlusRd(0x48)
  }
  
  implicit object dec2 extends DEC_1[O, r16] {
	 val opcode = OpcodePlusRd(0x48)
  }
}