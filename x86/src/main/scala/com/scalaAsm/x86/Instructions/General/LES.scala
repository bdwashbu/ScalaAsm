package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object LES extends InstructionDefinition[OneOpcode]("LES") with LESImpl

// Load Far Pointer

trait LESImpl {
  implicit object LES_0 extends LES._2[r16, m] {
    def opcode = 0xC4 /r
    override def hasImplicitOperand = true
  }

  implicit object LES_1 extends LES._2[r32, m] {
    def opcode = 0xC4 /r
    override def hasImplicitOperand = true
  }
}
