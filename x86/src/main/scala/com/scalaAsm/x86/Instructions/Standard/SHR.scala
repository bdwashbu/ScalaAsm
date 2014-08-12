package com.scalaAsm.x86
package Instructions
package Standard

trait SHR extends x86Instruction {
  val mnemonic = "SHR"
}

trait SHR_2[OpEn, -O1, -O2] extends TwoOperandInstruction[OpEn, O1,O2] with SHR

object SHR {
  
  implicit object shr1 extends SHR_2[MI, rm32, imm8] {
      val opcode = 0xC1 /+ 5
  }
}