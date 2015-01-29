package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Interrupt Flag
// Category: general/flgctrl

object STI extends InstructionDefinition[OneOpcode]("STI") with STIImpl

trait STIImpl {
  implicit object STI_0 extends STI._0 {
    def opcode = 0xFB
  }
}
