package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JECXZ extends InstructionDefinition[OneOpcode]("JECXZ") with JECXZImpl

// Jump short if eCX register is 0

trait JECXZImpl {
  implicit object JECXZ_0 extends JECXZ._1[rel8] {
    def opcode = 0xE3
    override def hasImplicitOperand = true
  }
}
