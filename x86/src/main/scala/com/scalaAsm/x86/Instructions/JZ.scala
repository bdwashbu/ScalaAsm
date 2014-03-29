package com.scalaAsm.x86
package Instructions

abstract class JZ extends x86Instruction("JZ")

trait JZ_1[-O1] extends JZ with OneOperandInstruction[O1] with InstructionFormat

object JZ {
  
  implicit object jz1 extends JZ_1[imm8] {
      def operands = new OneOperandFormat[imm8]() {
        def getAddressingForm(op1: imm8, opcode: OpcodeFormat) = Some(new AddressingFormSpecifier {
	        val modRM = None
		    val sib = None
		    val displacment = None
		    val immediate = Some(op1)
	     })}
      val opcode = OneOpcode(0x74)
  }
}