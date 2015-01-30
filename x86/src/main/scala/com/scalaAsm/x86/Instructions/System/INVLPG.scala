package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Invalidate TLB Entry
// Category: general

object INVLPG extends InstructionDefinition[OneOpcode]("INVLPG") with INVLPGImpl

trait INVLPGImpl {
  implicit object INVLPG_0 extends INVLPG._1[m] {
    def opcode = 0x1 /+ 7
  }
}
