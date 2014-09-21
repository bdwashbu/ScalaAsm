package com.scalaAsm.x86
package Instructions
package Standard

abstract class SBB_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode]("SBB")

object SBB_2 {

  implicit object sbb1 extends SBB_2[r32, rm32, RM] {
    def opcode = 0x1B
  }
}