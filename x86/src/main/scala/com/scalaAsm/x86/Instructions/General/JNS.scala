package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not sign (SF=0)
// Category: general/branch/cond

object JNS extends InstructionDefinition[OneOpcode]("JNS") with JNSImpl

trait JNSImpl {
  implicit object JNS_0 extends JNS._1[rel8] {
    def opcode = 0x79
  }
}
