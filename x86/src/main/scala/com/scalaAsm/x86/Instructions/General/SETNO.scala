package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Byte on Condition - not overflow (OF=0)
// Category: general/datamov

object SETNO extends InstructionDefinition[OneOpcode]("SETNO") with SETNOImpl

trait SETNOImpl {
  implicit object SETNO_0 extends SETNO._1[rm8] {
    def opcode = 0x91 /+ 0
  }
}
