package com.scalaAsm.x86
package Instructions
package Standard

abstract class XOR_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode]("XOR")

object XOR_2 {
  implicit object xor1 extends XOR_2[rm64, r64, MR] {
    def opcode = 0x31
    override def prefix = REX.W(true)
  }
}