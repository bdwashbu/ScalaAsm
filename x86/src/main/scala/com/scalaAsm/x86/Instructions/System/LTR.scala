package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Load Task Register
// Category: general

object LTR extends InstructionDefinition[OneOpcode]("LTR") with LTRImpl

trait LTRImpl {
  implicit object LTR_0 extends LTR._1[rm16] {
    def opcode = 0x0 /+ 3
    override def hasImplicitOperand = true
  }
}
