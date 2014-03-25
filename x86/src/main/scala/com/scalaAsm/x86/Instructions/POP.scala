package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._

abstract class POP extends x86Instruction("POP")

trait POP_1[-O1] extends POP with OneOperand[O1] with InstructionFormat

object POP {
  
  implicit object pop1 extends POP_1[r32] {
      def operands = O(op1)
      def opcode = OpcodePlusRd(0x58, op1)
  }
  
  implicit object pop2 extends POP_1[DS] {
      def operands = new OneOperandFormat[DS](op1) {def getAddressingForm(opcode: OpcodeFormat) = null}
      val opcode = OneOpcode(0x1F)
  }
}