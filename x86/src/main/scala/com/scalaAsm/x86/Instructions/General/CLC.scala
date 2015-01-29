package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CLC extends InstructionDefinition[OneOpcode]("CLC") with CLCImpl

// Clear Carry Flag

trait CLCImpl {
  implicit object CLC_0 extends CLC._0 {
    def opcode = 0xF8
  }
}
