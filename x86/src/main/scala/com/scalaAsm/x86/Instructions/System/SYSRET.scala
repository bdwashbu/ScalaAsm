package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Return From Fast System Call
// Category: general/branch/trans

object SYSRET extends InstructionDefinition[OneOpcode]("SYSRET") with SYSRETImpl

trait SYSRETImpl {
  implicit object SYSRET_0 extends SYSRET._0 {
    def opcode = 0x7
    override def hasImplicitOperand = true
  }
}
