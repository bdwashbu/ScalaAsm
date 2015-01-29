package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if below/not above or equal/carry (CF=1)
// Category: general/branch/cond

object JNAE extends InstructionDefinition[OneOpcode]("JNAE") with JNAEImpl

trait JNAEImpl {
  implicit object JNAE_0 extends JNAE._1[rel8] {
    def opcode = 0x72
  }
}
