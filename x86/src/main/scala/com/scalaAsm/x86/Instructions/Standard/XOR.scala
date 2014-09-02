package com.scalaAsm.x86
package Instructions
package Standard

trait XOR extends x86Instruction {
  val mnemonic = "XOR"
}

trait XOR_2[OpEn, -O1, -O2] extends TwoOperandInstruction[OpEn, O1,O2] with XOR

object XOR {
  implicit object xor1 extends XOR_2[MR, rm64, r64] {
      val opcode = OneOpcode(0x31)
  }
}