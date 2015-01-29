package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Decrement count; Jump short if count!=0
// Category: general/branch/cond

object LOOP extends InstructionDefinition[OneOpcode]("LOOP") with LOOPImpl

trait LOOPImpl {
  implicit object LOOP_0 extends LOOP._1[rel8] {
    def opcode = 0xE2
    override def hasImplicitOperand = true
  }
}
