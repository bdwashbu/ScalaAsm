package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Byte on Condition - not sign (SF=0)
// Category: general/datamov

object SETNS extends InstructionDefinition[OneOpcode]("SETNS") with SETNSImpl

trait SETNSImpl {
  implicit object SETNS_0 extends SETNS._1[rm8] {
    def opcode = 0x99 /+ 0
  }
}
