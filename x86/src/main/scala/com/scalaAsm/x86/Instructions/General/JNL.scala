package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not less/greater or equal (SF=OF)
// Category: general/branch/cond

object JNL extends InstructionDefinition[OneOpcode]("JNL") with JNLImpl

trait JNLImpl {
  implicit object JNL_0 extends JNL._1[rel8] {
    def opcode = 0x7D
  }

  implicit object JNL_1 extends JNL._1[rel16] {
    def opcode = 0x8D
  }

  implicit object JNL_2 extends JNL._1[rel32] {
    def opcode = 0x8D
  }
}
