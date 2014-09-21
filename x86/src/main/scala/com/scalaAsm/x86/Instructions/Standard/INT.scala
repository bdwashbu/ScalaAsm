package com.scalaAsm.x86
package Instructions
package Standard

abstract class INT_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode]("INT")

object INT_1 {

  implicit object int1 extends INT_1[imm8, I] {
    def opcode = 0xCD
  }
}