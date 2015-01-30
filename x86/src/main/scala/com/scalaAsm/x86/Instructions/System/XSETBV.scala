package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Extended Control Register
// Category: general

object XSETBV extends InstructionDefinition[OneOpcode]("XSETBV") with XSETBVImpl

trait XSETBVImpl {
  implicit object XSETBV_0 extends XSETBV._0 {
    def opcode = 0x1 /+ 2
    override def hasImplicitOperand = true
  }
}
