package com.scalaAsm.x86
package Instructions

abstract class JNZ extends x86Instruction("JNZ")

trait JNZ_1[-O1] extends JNZ with OneOperand[O1] with InstructionFormat

object JNZ {
  
  implicit object jnz1 extends JNZ_1[imm8] {
      def operands = new OneOperandFormat[imm8](op1) {
        def getAddressingForm(opcode: OpcodeFormat) = Some(new AddressingFormSpecifier {
	        val modRM = None
		    val sib = None
		    val displacment = None
		    val immediate = Some(op1)
	     })}
      val opcode = OneOpcode(0x75)
  }
}