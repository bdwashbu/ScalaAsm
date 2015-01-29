package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JNP extends InstructionDefinition[OneOpcode]("JNP") with JNPImpl

// Jump short if not parity/parity odd

trait JNPImpl {
  implicit object JNP_0 extends JNP._1[rel8] {
    def opcode = 0x7B
  }
}
