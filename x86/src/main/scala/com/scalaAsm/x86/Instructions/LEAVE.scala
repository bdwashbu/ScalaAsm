package com.scalaAsm.x86
package Instructions

trait LEAVE extends x86Instruction with ZeroOperandInstruction {
  val mnemonic = "LEAVE"
}

object LEAVE {
  
  implicit object lea1 extends LEAVE {
      def operands = NoOperand
      val opcode: OpcodeFormat = 0xC9
  }
}