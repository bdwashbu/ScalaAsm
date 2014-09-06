package com.scalaAsm.x86
package Instructions
package Standard

trait LEAVE extends ZeroOperandInstruction[OneOpcode] with x86Instruction {
  val mnemonic = "LEAVE"
}

object LEAVE {
  
  implicit object lea1 extends LEAVE {
      def opcode = 0xC9
  }
}