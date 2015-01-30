package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if overflow (OF=1)
// Category: general/branch/cond

object JO extends InstructionDefinition[OneOpcode]("JO") with JOImpl

trait JOImpl {
  implicit object JO_0 extends JO._1[rel8] {
    def opcode = 0x70
  }

  implicit object JO_1 extends JO._1[rel16] {
    def opcode = 0x80
  }

  implicit object JO_2 extends JO._1[rel32] {
    def opcode = 0x80
  }
}
