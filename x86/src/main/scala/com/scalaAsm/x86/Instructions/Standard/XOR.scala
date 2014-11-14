package com.scalaAsm.x86
package Instructions
package Standard

object XOR extends OperandInstruction[OneOpcode]("XOR") with xorLow
 
trait xorLow {
  implicit object xor1 extends XOR.TwoOps[rm64, r64, MR] {
    def opcode = 0x31
    override def prefix = REX.W(true)
  }
}
