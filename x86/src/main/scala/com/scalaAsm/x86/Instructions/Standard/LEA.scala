package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object LEA extends InstructionDefinition[OneOpcode]("LEA") with LEAImpl

trait LEAImpl {
  implicit object LEA_141_r16_m extends LEA._2[r16, m] {
    def opcode = 0x8D /r
  }

  implicit object LEA_141_r32_m extends LEA._2[r32, m] {
    def opcode = 0x8D /r
  }

  implicit object LEA_141_r64_m extends LEA._2[r64, m] {
    def opcode = 0x8D /r
    override def prefix = REX.W(true)
  }
}
