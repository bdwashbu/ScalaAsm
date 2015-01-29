package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not below/above or equal/not carry (CF=0)
// Category: general/branch/cond

object JNC extends InstructionDefinition[OneOpcode]("JNC") with JNCImpl

trait JNCImpl {
  implicit object JNC_0 extends JNC._1[rel8] {
    def opcode = 0x73
  }
}
