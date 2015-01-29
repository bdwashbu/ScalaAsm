package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JC extends InstructionDefinition[OneOpcode]("JC") with JCImpl

// Jump short if below/not above or equal/carry (CF=1)

trait JCImpl {
  implicit object JC_0 extends JC._1[rel8] {
    def opcode = 0x72
  }
}
