package com.scalaAsm.x86
package Instructions
package Standard

object DEC extends OperandInstruction[OneOpcode]("DEC") with decLow
  
trait decLow {
  implicit object dec1 extends DEC.OneOp[r32, O] {
    def opcode = 0x48 + rd
  }

  implicit object dec2 extends DEC.OneOp[r16, O] {
    def opcode = 0x48 + rw
  }
}