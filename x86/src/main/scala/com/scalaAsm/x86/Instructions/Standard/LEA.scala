package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object LEA extends InstructionDefinition[OneOpcode]("LEA") with LEAImpl

// Load Effective Address

trait LEAImpl {
  implicit object LEA_0 extends LEA._2[r16, m] {
    def opcode = 0x8D /r
  }

  implicit object LEA_1 extends LEA._2[r32, m] {
    def opcode = 0x8D /r
  }

  implicit object LEA_2 extends LEA._2[r64, m] {
    def opcode = 0x8D /r
    override def prefix = REX.W(true)
  }
}
