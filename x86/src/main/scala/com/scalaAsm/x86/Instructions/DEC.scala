package com.scalaAsm.x86
package Instructions

abstract class DEC extends x86Instruction("DEC")

trait DEC_1[-O1 <: Operand] extends DEC with OneOperandInstruction[O1]

object DEC {
  
  implicit object dec1 extends DEC_1[r32] {
     def opEn = O
     val opcode = OpcodePlusRd(0x48)
  }
  
  implicit object dec2 extends DEC_1[r16] {
     def opEn = O
	 val opcode = OpcodePlusRd(0x48)
  }
}