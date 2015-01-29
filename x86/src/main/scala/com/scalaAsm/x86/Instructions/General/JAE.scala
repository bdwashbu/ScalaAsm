package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JAE extends InstructionDefinition[OneOpcode]("JAE") with JAEImpl

// Jump short if not below/above or equal/not carry (CF=0)

trait JAEImpl {
  implicit object JAE_0 extends JAE._1[rel8] {
    def opcode = 0x73
  }
}
