package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JNL extends InstructionDefinition[OneOpcode]("JNL") with JNLImpl

// Jump short if not less/greater or equal (SF=OF)

trait JNLImpl {
  implicit object JNL_0 extends JNL._1[rel8] {
    def opcode = 0x7D
  }
}
