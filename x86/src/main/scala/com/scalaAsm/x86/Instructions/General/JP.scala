package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if parity/parity even (PF=1)
// Category: general/branch/cond

object JP extends InstructionDefinition[OneOpcode]("JP") with JPImpl

trait JPImpl {
  implicit object JP_0 extends JP._1[rel8] {
    def opcode = 0x7A
  }
}
