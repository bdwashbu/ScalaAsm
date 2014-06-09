package com.scalaAsm.x86
package Instructions

trait NOT extends x86Instruction {
  val mnemonic = "NOT"
}

trait NOT_1[OpEn, -O1 <: Operand] extends OneOperandInstruction[OpEn, O1] with NOT

object NOT {
  
  implicit object not1 extends NOT_1[M, rm32] {
      val opcode = 0xF7 /+ 2
  }
}