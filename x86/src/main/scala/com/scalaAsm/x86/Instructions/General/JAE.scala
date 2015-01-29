package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not below/above or equal/not carry (CF=0)
// Category: general/branch/cond

object JAE extends InstructionDefinition[OneOpcode]("JAE") with JAEImpl

trait JAEImpl {
  implicit object JAE_0 extends JAE._1[rel8] {
    def opcode = 0x73
  }
}
