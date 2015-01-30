package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Save Processor Extended States
// Category: general

object XSAVE extends InstructionDefinition[OneOpcode]("XSAVE") with XSAVEImpl

trait XSAVEImpl {
  implicit object XSAVE_0 extends XSAVE._1[m] {
    def opcode = 0xAE /+ 4
    override def hasImplicitOperand = true
  }
}
