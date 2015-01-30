package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not parity/parity odd
// Category: general/branch/cond

object JNP extends InstructionDefinition[OneOpcode]("JNP") with JNPImpl

trait JNPImpl {
  implicit object JNP_0 extends JNP._1[rel8] {
    def opcode = 0x7B
  }

  implicit object JNP_1 extends JNP._1[rel16] {
    def opcode = 0x8B
  }

  implicit object JNP_2 extends JNP._1[rel32] {
    def opcode = 0x8B
  }
}
