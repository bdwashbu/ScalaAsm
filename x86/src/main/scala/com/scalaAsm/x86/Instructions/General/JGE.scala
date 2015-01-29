package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not less/greater or equal (SF=OF)
// Category: general/branch/cond

object JGE extends InstructionDefinition[OneOpcode]("JGE") with JGEImpl

trait JGEImpl {
  implicit object JGE_0 extends JGE._1[rel8] {
    def opcode = 0x7D
  }
}
