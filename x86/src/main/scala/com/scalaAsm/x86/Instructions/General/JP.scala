package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JP extends InstructionDefinition[OneOpcode]("JP") with JPImpl

// Jump short if parity/parity even (PF=1)

trait JPImpl {
  implicit object JP_0 extends JP._1[rel8] {
    def opcode = 0x7A
  }
}
