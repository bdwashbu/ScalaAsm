package com.scalaAsm.x86
package Instructions
package Standard

trait TEST extends x86Instruction {
  val mnemonic = "TEST"
}

trait TEST_2[OpEn, -O1, -O2] extends TwoOperandInstruction[OpEn, O1,O2] with TEST

object TEST {
  
  implicit object test1 extends TEST_2[RM, r32, rm32] {
      val opcode: OpcodeFormat = 0x85
  }
  
  implicit object test2 extends TEST_2[MI, rm32, imm32] {
      val opcode: OpcodeFormat = 0xF7 /+ 0
  }
}