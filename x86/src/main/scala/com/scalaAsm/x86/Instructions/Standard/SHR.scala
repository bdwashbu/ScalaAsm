package com.scalaAsm.x86
package Instructions
package Standard

trait SHR extends x86Instruction {
  val mnemonic = "SHR"
}

trait SHR_2[-O1, -O2, OpEn[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn] with SHR

object SHR {
  
  implicit object shr1 extends SHR_2[rm32, imm8, MI] {
      val opcode = 0xC1 /+ 5
  }
}