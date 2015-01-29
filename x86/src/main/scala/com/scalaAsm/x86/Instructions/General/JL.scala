package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if less/not greater (SF!=OF)
// Category: general/branch/cond

object JL extends InstructionDefinition[OneOpcode]("JL") with JLImpl

trait JLImpl {
  implicit object JL_0 extends JL._1[rel8] {
    def opcode = 0x7C
  }
}
