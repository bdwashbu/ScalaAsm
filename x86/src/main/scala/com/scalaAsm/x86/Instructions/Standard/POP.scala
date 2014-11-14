package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object POP extends OperandInstruction[OneOpcode]("POP") with popLow
 
trait popLow {

  implicit object pop1 extends POP.OneOp[r32, O] {
    def opcode = 0x58 + rd
  }

  implicit object pop3 extends POP.OneOp[r64, O] {
    def opcode = 0x58 + rd
    override val defaultsTo64Bit = true
  }

  implicit object pop2 extends POP.OneOp[DS, DSFormat] {
    def opcode = 0x1F
  }
}
