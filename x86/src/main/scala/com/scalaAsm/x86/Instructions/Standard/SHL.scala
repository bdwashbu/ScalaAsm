package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.One

abstract class SHL_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode]("SHL")

object SHL_2 {

  implicit object shl1 extends SHL_2[rm8, One, M1] {
    def opcode = 0xD0 /+ 4
  }
}
