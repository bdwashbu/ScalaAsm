package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Byte on Condition - less or equal/not greater ((ZF=1) OR (SF!=OF))
// Category: general/datamov

object SETNG extends InstructionDefinition[OneOpcode]("SETNG") with SETNGImpl

trait SETNGImpl {
  implicit object SETNG_0 extends SETNG._1[rm8] {
    def opcode = 0x9E /+ 0
  }
}
