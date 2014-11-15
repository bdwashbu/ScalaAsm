package com.scalaAsm.x86
package Instructions
package Standard

object DEC extends InstructionDefinition[OneOpcode]("DEC") with decLow
  
trait decLow {
  implicit object dec1 extends DEC._1[r32, O] {
    def opcode = 0x48 + rd
  }

  implicit object dec2 extends DEC._1[r16, O] {
    def opcode = 0x48 + rw
  }
}