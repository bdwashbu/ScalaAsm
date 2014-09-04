package com.scalaAsm.x86
package Instructions
package Standard

trait MUL extends x86Instruction {
  val mnemonic = "MUL"
}

trait MUL_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn] with MUL

object MUL {
  
  implicit object mul1 extends MUL_1[rm32, M] {
      val opcode = 0xF7 /+ 4
  }
}