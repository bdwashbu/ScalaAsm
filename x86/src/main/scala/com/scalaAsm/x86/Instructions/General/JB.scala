package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JB extends InstructionDefinition[OneOpcode]("JB") with JBImpl

// Jump short if below/not above or equal/carry (CF=1)

trait JBImpl {
  implicit object JB_0 extends JB._1[rel8] {
    def opcode = 0x72
  }
}
