package com.scalaAsm.x86
package Instructions

abstract class JNZ extends x86Instruction("JNZ")

trait JNZ_1[-O1] extends JNZ with OneOperand[O1] with InstructionFormat

object JNZ {
  
  implicit object jnz1 extends JNZ_1[imm8] {
      def operands = new OneOperandFormat[imm8](x) {def getAddressingForm = null}
      val opcode = OneOpcode(0x75)
  }
}