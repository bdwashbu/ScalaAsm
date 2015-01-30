package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Restore Processor Extended States
// Category: general

object XRSTOR extends InstructionDefinition[OneOpcode]("XRSTOR") with XRSTORImpl

trait XRSTORImpl {
  implicit object XRSTOR_0 extends XRSTOR._1[m] {
    def opcode = 0xAE /+ 5
    override def hasImplicitOperand = true
  }
}
