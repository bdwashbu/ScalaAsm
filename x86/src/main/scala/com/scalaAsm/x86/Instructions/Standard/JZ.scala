package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JZ extends InstructionDefinition[OneOpcode]("JZ") with JZImpl

// Jump short if zero/equal (ZF=0)

trait JZImpl {
  implicit object JZ_0 extends JZ._1[rel8] {
    def opcode = 0x74
  }
}
