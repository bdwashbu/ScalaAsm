package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Decrement Stack-Top Pointer
// Category: general/control

object FDECSTP extends InstructionDefinition[OneOpcode]("FDECSTP") with FDECSTPImpl

trait FDECSTPImpl {
  implicit object FDECSTP_0 extends FDECSTP._0 {
    def opcode = 0xD9 /+ 6
  }
}
