package com.scalaAsm.x86
package Instructions

abstract class LEAVE extends x86Instruction("LEAVE") with ZeroOperandInstruction

object LEAVE {
  
  implicit object lea1 extends LEAVE {
      def operands = NoOperand
      val opcode: OpcodeFormat = 0xC9
  }
}