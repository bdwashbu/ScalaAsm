package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JE extends InstructionDefinition[OneOpcode]("JE") with JEImpl

// Jump short if zero/equal (ZF=0)

trait JEImpl {
  implicit object JE_0 extends JE._1[rel8] {
    def opcode = 0x74
  }
}
