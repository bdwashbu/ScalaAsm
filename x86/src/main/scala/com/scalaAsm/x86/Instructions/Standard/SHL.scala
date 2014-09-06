package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.One

trait SHL extends x86Instruction {
  val mnemonic = "SHL"
}

trait SHL_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode] with SHL

object SHL {

  implicit object shl1 extends SHL_2[rm8, One, M1] {
    def opcode = 0xD0 /+ 4
  }
}
