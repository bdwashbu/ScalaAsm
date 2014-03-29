package com.scalaAsm.x86
package Instructions

abstract class LEAVE extends x86Instruction("LEAVE") with ZeroOperandInstruction with InstructionFormat

object LEAVE {
  
  implicit object lea1 extends LEAVE {
      def operands = new NoOperand{}
      val opcode: OpcodeFormat = 0xC9
  }
}