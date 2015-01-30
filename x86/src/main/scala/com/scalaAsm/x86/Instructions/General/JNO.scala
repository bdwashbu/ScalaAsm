package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not overflow (OF=0)
// Category: general/branch/cond

object JNO extends InstructionDefinition[OneOpcode]("JNO") with JNOImpl

trait JNOImpl {
  implicit object JNO_0 extends JNO._1[rel8] {
    def opcode = 0x71
  }

  implicit object JNO_1 extends JNO._1[rel16] {
    def opcode = 0x81
  }

  implicit object JNO_2 extends JNO._1[rel32] {
    def opcode = 0x81
  }
}
