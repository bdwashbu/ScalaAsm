package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Initialize Floating-Point Unit
// Category: general/control

object FINIT extends InstructionDefinition[OneOpcode]("FINIT") with FINITImpl

trait FINITImpl {
  implicit object FINIT_0 extends FINIT._0 {
    def opcode = 0xDB /+ 4
  }
}
