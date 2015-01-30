package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Load Machine Status Word
// Category: general

object LMSW extends InstructionDefinition[OneOpcode]("LMSW") with LMSWImpl

trait LMSWImpl {
  implicit object LMSW_0 extends LMSW._1[rm16] {
    def opcode = 0x1 /+ 6
    override def hasImplicitOperand = true
  }
}
