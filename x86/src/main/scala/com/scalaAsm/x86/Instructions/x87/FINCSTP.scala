package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Increment Stack-Top Pointer
// Category: general/control

object FINCSTP extends InstructionDefinition[OneOpcode]("FINCSTP") with FINCSTPImpl

trait FINCSTPImpl {
  implicit object FINCSTP_0 extends FINCSTP._0 {
    def opcode = 0xD9 /+ 6
  }
}
