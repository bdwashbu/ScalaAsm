package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Byte on Condition - zero/equal (ZF=0)
// Category: general/datamov

object SETE extends InstructionDefinition[OneOpcode]("SETE") with SETEImpl

trait SETEImpl {
  implicit object SETE_0 extends SETE._1[rm8] {
    def opcode = 0x94 /+ 0
  }
}
