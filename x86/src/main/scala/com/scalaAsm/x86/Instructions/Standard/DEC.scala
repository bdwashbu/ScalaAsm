package com.scalaAsm.x86
package Instructions
package Standard

abstract class DEC_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode]("DEC")

object DEC_1 {

  implicit object dec1 extends DEC_1[r32, O] {
    def opcode = 0x48 + rd
  }

  implicit object dec2 extends DEC_1[r16, O] {
    def opcode = 0x48 + rw
  }
}