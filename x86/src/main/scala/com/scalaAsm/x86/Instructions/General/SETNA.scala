package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Byte on Condition - below or equal/not above (CF=1 AND ZF=1)
// Category: general/datamov

object SETNA extends InstructionDefinition[OneOpcode]("SETNA") with SETNAImpl

trait SETNAImpl {
  implicit object SETNA_0 extends SETNA._1[rm8] {
    def opcode = 0x96 /+ 0
  }
}
