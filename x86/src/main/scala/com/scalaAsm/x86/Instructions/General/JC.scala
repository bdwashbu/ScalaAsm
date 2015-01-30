package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if below/not above or equal/carry (CF=1)
// Category: general/branch/cond

object JC extends InstructionDefinition[OneOpcode]("JC") with JCImpl

trait JCImpl {
  implicit object JC_0 extends JC._1[rel8] {
    def opcode = 0x72
  }

  implicit object JC_1 extends JC._1[rel16] {
    def opcode = 0x82
  }

  implicit object JC_2 extends JC._1[rel32] {
    def opcode = 0x82
  }
}
