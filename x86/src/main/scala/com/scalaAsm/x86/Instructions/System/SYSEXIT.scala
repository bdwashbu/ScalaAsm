package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Fast Return from Fast System Call
// Category: general/branch/trans

object SYSEXIT extends InstructionDefinition[OneOpcode]("SYSEXIT") with SYSEXITImpl

trait SYSEXITImpl {
  implicit object SYSEXIT_0 extends SYSEXIT._0 {
    def opcode = 0x35
    override def hasImplicitOperand = true
  }
}
