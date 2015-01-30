package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Load Far Pointer
// Category: general/datamovsegreg

object LFS extends InstructionDefinition[OneOpcode]("LFS") with LFSImpl

trait LFSImpl {
  implicit object LFS_0 extends LFS._2[r16, m] {
    def opcode = 0xB4 /r
    override def hasImplicitOperand = true
  }

  implicit object LFS_1 extends LFS._2[r32, m] {
    def opcode = 0xB4 /r
    override def hasImplicitOperand = true
  }

  implicit object LFS_2 extends LFS._2[r64, m] {
    def opcode = 0xB4 /r
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}
