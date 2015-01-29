package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Convert
// Category: general/conver

object CWDE extends InstructionDefinition[OneOpcode]("CWDE") with CWDEImpl

trait CWDEImpl {
  implicit object CWDE_0 extends CWDE._0 {
    def opcode = 0x98
    override def hasImplicitOperand = true
  }
}