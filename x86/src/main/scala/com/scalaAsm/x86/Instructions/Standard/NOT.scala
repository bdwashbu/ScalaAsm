package com.scalaAsm.x86
package Instructions
package Standard

trait NOT extends x86Instruction {
  val mnemonic = "NOT"
}

trait NOT_1[-O1, OpEn[O1]] extends OneOperandInstruction[O1, OpEn] with NOT

object NOT {
  
  implicit object not1 extends NOT_1[rm32, M] {
      val opcode = 0xF7 /+ 2
  }
}