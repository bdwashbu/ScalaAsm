package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Clear Task-Switched Flag in CR0
// Category: general

object CLTS extends InstructionDefinition[OneOpcode]("CLTS") with CLTSImpl

trait CLTSImpl {
  implicit object CLTS_0 extends CLTS._0 {
    def opcode = 0x6
    override def hasImplicitOperand = true
  }
}
