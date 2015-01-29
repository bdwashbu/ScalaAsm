package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JO extends InstructionDefinition[OneOpcode]("JO") with JOImpl

// Jump short if overflow (OF=1)

trait JOImpl {
  implicit object JO_0 extends JO._1[rel8] {
    def opcode = 0x70
  }
}
