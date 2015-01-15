package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JZ extends InstructionDefinition[OneOpcode]("JZ") with JZImpl

trait JZImpl {
  implicit object JZ_116_rel8 extends JZ._1[rel8] {
    def opcode = 0x74
  }
}
