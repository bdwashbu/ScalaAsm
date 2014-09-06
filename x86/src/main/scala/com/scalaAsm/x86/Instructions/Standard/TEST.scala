package com.scalaAsm.x86
package Instructions
package Standard

trait TEST extends x86Instruction {
  val mnemonic = "TEST"
}

trait TEST_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode] with TEST

object TEST {
  
  implicit object test1 extends TEST_2[r32, rm32, RM] {
      val opcode = OneOpcode(0x85)
  }
  
  implicit object test2 extends TEST_2[rm32, imm32, MI] {
      val opcode = OneOpcode(0xF7) /+ 0
  }
}