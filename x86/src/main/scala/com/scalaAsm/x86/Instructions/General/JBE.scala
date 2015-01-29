package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if below or equal/not above (CF=1 AND ZF=1)
// Category: general/branch/cond

object JBE extends InstructionDefinition[OneOpcode]("JBE") with JBEImpl

trait JBEImpl {
  implicit object JBE_0 extends JBE._1[rel8] {
    def opcode = 0x76
  }
}
