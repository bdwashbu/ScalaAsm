package com.scalaAsm.x86
package Instructions
package Standard

abstract class TEST_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode]("TEST")

object TEST_2 {

  implicit object test1 extends TEST_2[r32, rm32, RM] {
    def opcode = 0x85
  }

  implicit object test2 extends TEST_2[rm32, imm32, MI] {
    def opcode = 0xF7 /+ 0
  }
}