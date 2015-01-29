package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JPO extends InstructionDefinition[OneOpcode]("JPO") with JPOImpl

// Jump short if not parity/parity odd

trait JPOImpl {
  implicit object JPO_0 extends JPO._1[rel8] {
    def opcode = 0x7B
  }
}
