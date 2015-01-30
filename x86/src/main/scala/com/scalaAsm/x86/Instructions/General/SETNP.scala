package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Byte on Condition - not parity/parity odd
// Category: general/datamov

object SETNP extends InstructionDefinition[OneOpcode]("SETNP") with SETNPImpl

trait SETNPImpl {
  implicit object SETNP_0 extends SETNP._1[rm8] {
    def opcode = 0x9B /+ 0
  }
}
