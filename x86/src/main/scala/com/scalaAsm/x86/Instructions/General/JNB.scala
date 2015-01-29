package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JNB extends InstructionDefinition[OneOpcode]("JNB") with JNBImpl

// Jump short if not below/above or equal/not carry (CF=0)

trait JNBImpl {
  implicit object JNB_0 extends JNB._1[rel8] {
    def opcode = 0x73
  }
}
