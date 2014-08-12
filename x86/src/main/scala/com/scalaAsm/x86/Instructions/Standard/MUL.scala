package com.scalaAsm.x86
package Instructions
package Standard

trait MUL extends x86Instruction {
  val mnemonic = "MUL"
}

trait MUL_1[OpEn, -O1] extends OneOperandInstruction[OpEn, O1] with MUL

object MUL {
  
  implicit object mul1 extends MUL_1[M, rm32] {
      val opcode = 0xF7 /+ 4
  }
}