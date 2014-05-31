package com.scalaAsm.x86
package Instructions

trait JZ extends x86Instruction {
  val mnemonic = "JZ"
}

trait JZ_1[-O1 <: Operand] extends JZ with OneOperandInstruction[O1]

object JZ {
  
  implicit object jz1 extends JZ_1[imm8] {
      def opEn = new OneOperandFormat[imm8]() {
        def getAddressingForm(op1: imm8, opcode: OpcodeFormat) = new AddressingFormSpecifierTemp {
	        val addressingForm = NoModRM()
		    val displacment = None
		    val immediate = Some(op1)
	     }}
      val opcode = OneOpcode(0x74)
  }
}