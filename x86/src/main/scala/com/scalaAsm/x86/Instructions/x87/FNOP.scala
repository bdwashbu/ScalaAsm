package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: No Operation
// Category: general/control

object FNOP extends InstructionDefinition[OneOpcode]("FNOP") with FNOPImpl

trait FNOPImpl {
  implicit object FNOP_0 extends FNOP._0 {
    def opcode = 0xD9 /+ 2
  }
}
