package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object POP extends InstructionDefinition[OneOpcode]("POP") with popLow
 
trait popLow {

  implicit object pop1 extends POP._1[r32, O] {
    def opcode = 0x58 + rd
  }

  implicit object pop3 extends POP._1[r64, O] {
    def opcode = 0x58 + rd
    override val defaultsTo64Bit = true
  }

  implicit object pop2 extends POP._1[DS, DSFormat] {
    def opcode = 0x1F
  }
}
