package com.scalaAsm.x86
package Instructions
package Standard

trait XOR extends x86Instruction {
  val mnemonic = "XOR"
}

trait XOR_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode] with XOR

object XOR {
  implicit object xor1 extends XOR_2[rm64, r64, MR] {
    def opcode = 0x31
  }
}