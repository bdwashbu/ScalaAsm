package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.One

object SHL extends InstructionDefinition[OneOpcode]("SHL") with shlLow
 
trait shlLow {
  implicit object shl1 extends SHL._2[rm8, One, M1] {
    def opcode = 0xD0 /+ 4
  }
}
