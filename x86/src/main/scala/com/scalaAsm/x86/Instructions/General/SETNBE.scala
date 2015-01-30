package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Byte on Condition - not below or equal/above (CF=0 AND ZF=0)
// Category: general/datamov

object SETNBE extends InstructionDefinition[OneOpcode]("SETNBE") with SETNBEImpl

trait SETNBEImpl {
  implicit object SETNBE_0 extends SETNBE._1[rm8] {
    def opcode = 0x97 /+ 0
  }
}
