package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Load Far Pointer
// Category: general/datamovsegreg

object LSS extends InstructionDefinition[OneOpcode]("LSS") with LSSImpl

trait LSSImpl {
  implicit object LSS_0 extends LSS._2[r16, m] {
    def opcode = 0xB2 /r
    override def hasImplicitOperand = true
  }

  implicit object LSS_1 extends LSS._2[r32, m] {
    def opcode = 0xB2 /r
    override def hasImplicitOperand = true
  }

  implicit object LSS_2 extends LSS._2[r64, m] {
    def opcode = 0xB2 /r
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}
