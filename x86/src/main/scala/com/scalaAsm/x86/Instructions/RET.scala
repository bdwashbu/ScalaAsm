package com.scalaAsm.x86
package Instructions

abstract class RET extends x86Instruction with ZeroOperandInstruction {
  val mnemonic = "RET"
}

object RET {

  implicit object NearReturn extends RET {
      def operands = NoOperand
      val opcode: OpcodeFormat = 0xC3
  }
}