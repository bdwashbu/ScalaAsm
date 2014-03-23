package com.scalaAsm.x86
package Instructions

abstract class JZ extends x86Instruction("JZ")

trait JZ_1[-O1] extends JZ with OneOperand[O1] with InstructionFormat

object JZ {
  
  implicit object jz1 extends JZ_1[imm8] {
      def operands = new OneOperandFormat[imm8](x) {
        def getAddressingForm = Some(new AddressingFormSpecifier {
	        val modRM = None
		    val sib = None
		    val displacment = None
		    val immediate = Some(x)
	     })}
      val opcode = OneOpcode(0x74)
  }
}