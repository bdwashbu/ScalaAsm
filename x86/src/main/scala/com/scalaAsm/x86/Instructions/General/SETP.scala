package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Byte on Condition - parity/parity even (PF=1)
// Category: general/datamov

object SETP extends InstructionDefinition[OneOpcode]("SETP") with SETPImpl

trait SETPImpl {
  implicit object SETP_0 extends SETP._1[rm8] {
    def opcode = 0x9A /+ 0
  }
}
