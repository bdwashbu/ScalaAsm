package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not zero/not equal (ZF=1)
// Category: general/branch/cond

object JNZ extends InstructionDefinition[OneOpcode]("JNZ") with JNZImpl

trait JNZImpl {
  implicit object JNZ_0 extends JNZ._1[rel8] {
    def opcode = 0x75
  }
}
