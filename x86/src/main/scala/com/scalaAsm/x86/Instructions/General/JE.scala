package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if zero/equal (ZF=0)
// Category: general/branch/cond

object JE extends InstructionDefinition[OneOpcode]("JE") with JEImpl

trait JEImpl {
  implicit object JE_0 extends JE._1[rel8] {
    def opcode = 0x74
  }
}
