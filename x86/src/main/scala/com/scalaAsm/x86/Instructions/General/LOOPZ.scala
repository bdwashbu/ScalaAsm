package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Decrement count; Jump short if count!=0 and ZF=1
// Category: general/branch/cond

object LOOPZ extends InstructionDefinition[OneOpcode]("LOOPZ") with LOOPZImpl

trait LOOPZImpl {
  implicit object LOOPZ_0 extends LOOPZ._1[rel8] {
    def opcode = 0xE1
    override def hasImplicitOperand = true
  }
}
