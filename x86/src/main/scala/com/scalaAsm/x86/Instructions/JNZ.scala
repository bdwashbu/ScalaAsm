package com.scalaAsm.x86
package Instructions

abstract class JNZ extends x86Instruction("JNZ")

trait JNZ_1[-O1 <: Operand] extends JNZ with OneOperandInstruction[O1]

object JNZ {
  
  implicit object jnz1 extends JNZ_1[imm8] {
      def opEn = new OneOperandFormat[imm8]() {
        def getAddressingForm(op1: imm8, opcode: OpcodeFormat) = new AddressingFormSpecifierTemp {
	        val addressingForm = NoModRM()
		    val displacment = None
		    val immediate = Some(op1)
	     }}
      val opcode = OneOpcode(0x75)
  }
}