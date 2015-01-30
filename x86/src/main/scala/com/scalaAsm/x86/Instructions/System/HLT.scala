package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Halt
// Category: general

object HLT extends InstructionDefinition[OneOpcode]("HLT") with HLTImpl

trait HLTImpl {
  implicit object HLT_0 extends HLT._0 {
    def opcode = 0xF4
  }
}
