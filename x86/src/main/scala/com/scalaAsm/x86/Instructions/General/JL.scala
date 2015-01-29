package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JL extends InstructionDefinition[OneOpcode]("JL") with JLImpl

// Jump short if less/not greater (SF!=OF)

trait JLImpl {
  implicit object JL_0 extends JL._1[rel8] {
    def opcode = 0x7C
  }
}
