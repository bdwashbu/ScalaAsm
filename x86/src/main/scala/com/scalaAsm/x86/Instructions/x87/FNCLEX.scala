package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Clear Exceptions
// Category: general/control

object FNCLEX extends InstructionDefinition[OneOpcode]("FNCLEX") with FNCLEXImpl

trait FNCLEXImpl {
  implicit object FNCLEX_0 extends FNCLEX._0 {
    def opcode = 0xDB /+ 4
  }
}
