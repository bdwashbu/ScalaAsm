package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JPE extends InstructionDefinition[OneOpcode]("JPE") with JPEImpl

// Jump short if parity/parity even (PF=1)

trait JPEImpl {
  implicit object JPE_0 extends JPE._1[rel8] {
    def opcode = 0x7A
  }
}
