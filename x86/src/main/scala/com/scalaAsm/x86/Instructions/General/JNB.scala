package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not below/above or equal/not carry (CF=0)
// Category: general/branch/cond

object JNB extends InstructionDefinition[OneOpcode]("JNB") with JNBImpl

trait JNBImpl {
  implicit object JNB_0 extends JNB._1[rel8] {
    def opcode = 0x73
  }

  implicit object JNB_1 extends JNB._1[rel16] {
    def opcode = 0x83
  }

  implicit object JNB_2 extends JNB._1[rel32] {
    def opcode = 0x83
  }
}
