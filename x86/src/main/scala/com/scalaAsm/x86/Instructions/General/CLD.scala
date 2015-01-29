package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Clear Direction Flag
// Category: general/flgctrl

object CLD extends InstructionDefinition[OneOpcode]("CLD") with CLDImpl

trait CLDImpl {
  implicit object CLD_0 extends CLD._0 {
    def opcode = 0xFC
  }
}
