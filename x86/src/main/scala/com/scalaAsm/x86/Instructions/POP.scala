package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands.x86Registers._

abstract class POP extends x86Instruction("POP")

trait POP_1[-O1] extends POP with OneOperand[O1] with OperandEncoding

object POP {
  
  implicit object pop1 extends POP_1[r32] {
      def operands = O(x)
      def opcode = OpcodePlusRd(0x58, x)
  }
  
  implicit object pop2 extends POP_1[DS] {
      def operands = new OneOperandFormat[DS](x) {def getAddressingForm = null}
      val opcode = OneOpcode(0x1F)
  }
}