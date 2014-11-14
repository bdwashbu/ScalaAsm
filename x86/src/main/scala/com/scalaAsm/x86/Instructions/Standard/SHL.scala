package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.One

object SHL extends OperandInstruction[OneOpcode]("SHL") with shlLow
 
trait shlLow {
  implicit object shl1 extends SHL.TwoOps[rm8, One, M1] {
    def opcode = 0xD0 /+ 4
  }
}
