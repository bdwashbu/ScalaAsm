package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Carry Flag
// Category: general/flgctrl

object STC extends InstructionDefinition[OneOpcode]("STC") with STCImpl

trait STCImpl {
  implicit object STC_0 extends STC._0 {
    def opcode = 0xF9
  }
}
