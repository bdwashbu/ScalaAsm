package com.scalaAsm.x86
package Instructions

trait LEAVE extends ZeroOperandInstruction with x86Instruction {
  val mnemonic = "LEAVE"
}

object LEAVE {
  
  implicit object lea1 extends LEAVE {
      val opcode: OpcodeFormat = 0xC9
  }
}