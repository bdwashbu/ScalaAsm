package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Adjust RPL Field of Segment Selector
// Category: general

object ARPL extends InstructionDefinition[OneOpcode]("ARPL") with ARPLImpl

trait ARPLImpl {
  implicit object ARPL_0 extends ARPL._2[rm16, r16] {
    def opcode = 0x63 /r
  }
}
