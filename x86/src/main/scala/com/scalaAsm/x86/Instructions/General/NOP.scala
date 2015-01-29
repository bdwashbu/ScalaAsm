package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: No Operation
// Category: general/control

object NOP extends InstructionDefinition[OneOpcode]("NOP") with NOPImpl

trait NOPLow {
  implicit object NOP_0 extends NOP._1[rm16] {
    def opcode = 0x1F /+ 0
  }

  implicit object NOP_1 extends NOP._1[rm32] {
    def opcode = 0x1F /+ 0
  }
}

trait NOPImpl extends NOPLow {
  implicit object NOP_2 extends NOP._0 {
    def opcode = 0x90
  }
}
