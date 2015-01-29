package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JS extends InstructionDefinition[OneOpcode]("JS") with JSImpl

// Jump short if sign (SF=1)

trait JSImpl {
  implicit object JS_0 extends JS._1[rel8] {
    def opcode = 0x78
  }
}
