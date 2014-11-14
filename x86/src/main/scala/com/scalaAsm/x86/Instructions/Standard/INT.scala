package com.scalaAsm.x86
package Instructions
package Standard

object INT extends OperandInstruction[OneOpcode]("INT") with intLow
 
trait intLow {
  implicit object int1 extends INT.OneOp[imm8, I] {
    def opcode = 0xCD
  }
}