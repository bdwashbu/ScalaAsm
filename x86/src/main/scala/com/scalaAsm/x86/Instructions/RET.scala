package com.scalaAsm.x86
package Instructions

abstract class RET extends ZeroOperandInstruction with x86Instruction {
  val mnemonic = "RET"
}

object RET {

  implicit object NearReturn extends RET {
      val opcode: OpcodeFormat = 0xC3
  }
}