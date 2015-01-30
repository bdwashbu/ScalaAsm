package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Fast System Call
// Category: general/branch

object SYSENTER extends InstructionDefinition[OneOpcode]("SYSENTER") with SYSENTERImpl

trait SYSENTERImpl {
  implicit object SYSENTER_0 extends SYSENTER._0 {
    def opcode = 0x34
    override def hasImplicitOperand = true
  }
}
