package com.scalaAsm.x86
package Instructions
package Standard

trait DEC extends x86Instruction {
  val mnemonic = "DEC"
}

trait DEC_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode] with DEC

object DEC {
  
  implicit object dec1 extends DEC_1[r32, O] {
     val opcode = OneOpcode(0x48) + rd
  }
  
  implicit object dec2 extends DEC_1[r16, O] {
	 val opcode = OneOpcode(0x48) + rw
  }
}