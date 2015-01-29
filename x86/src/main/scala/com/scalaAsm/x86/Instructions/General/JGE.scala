package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JGE extends InstructionDefinition[OneOpcode]("JGE") with JGEImpl

// Jump short if not less/greater or equal (SF=OF)

trait JGEImpl {
  implicit object JGE_0 extends JGE._1[rel8] {
    def opcode = 0x7D
  }
}
