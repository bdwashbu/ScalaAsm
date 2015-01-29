package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JNO extends InstructionDefinition[OneOpcode]("JNO") with JNOImpl

// Jump short if not overflow (OF=0)

trait JNOImpl {
  implicit object JNO_0 extends JNO._1[rel8] {
    def opcode = 0x71
  }
}
