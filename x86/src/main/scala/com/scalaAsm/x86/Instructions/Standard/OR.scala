package com.scalaAsm.x86
package Instructions
package Standard

trait OR extends x86Instruction {
  val mnemonic = "OR"
}

trait OR_2[OpEn, -O1, -O2] extends TwoOperandInstruction[OpEn, O1,O2] with OR

object OR {
  
  implicit object or1 extends OR_2[MI, rm8, imm8] {
      val opcode = 0x80 /+ 1
  }
}