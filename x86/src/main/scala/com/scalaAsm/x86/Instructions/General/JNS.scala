package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JNS extends InstructionDefinition[OneOpcode]("JNS") with JNSImpl

// Jump short if not sign (SF=0)

trait JNSImpl {
  implicit object JNS_0 extends JNS._1[rel8] {
    def opcode = 0x79
  }
}
