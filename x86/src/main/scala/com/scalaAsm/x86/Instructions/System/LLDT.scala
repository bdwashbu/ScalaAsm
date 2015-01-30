package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Load Local Descriptor Table Register
// Category: general

object LLDT extends InstructionDefinition[OneOpcode]("LLDT") with LLDTImpl

trait LLDTImpl {
  implicit object LLDT_0 extends LLDT._1[rm16] {
    def opcode = 0x0 /+ 2
    override def hasImplicitOperand = true
  }
}
