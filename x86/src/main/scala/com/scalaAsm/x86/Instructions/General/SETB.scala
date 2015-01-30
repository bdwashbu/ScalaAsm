package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Byte on Condition - below/not above or equal/carry (CF=1)
// Category: general/datamov

object SETB extends InstructionDefinition[OneOpcode]("SETB") with SETBImpl

trait SETBImpl {
  implicit object SETB_0 extends SETB._1[rm8] {
    def opcode = 0x92 /+ 0
  }
}
