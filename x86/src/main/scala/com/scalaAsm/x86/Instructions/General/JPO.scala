package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not parity/parity odd
// Category: general/branch/cond

object JPO extends InstructionDefinition[OneOpcode]("JPO") with JPOImpl

trait JPOImpl {
  implicit object JPO_0 extends JPO._1[rel8] {
    def opcode = 0x7B
  }
}
