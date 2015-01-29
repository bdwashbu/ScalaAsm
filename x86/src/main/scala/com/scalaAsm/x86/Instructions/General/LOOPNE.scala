package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object LOOPNE extends InstructionDefinition[OneOpcode]("LOOPNE") with LOOPNEImpl

// Decrement count; Jump short if count!=0 and ZF=0

trait LOOPNEImpl {
  implicit object LOOPNE_0 extends LOOPNE._1[rel8] {
    def opcode = 0xE0
    override def hasImplicitOperand = true
  }
}
