package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JRCXZ extends InstructionDefinition[OneOpcode]("JRCXZ") with JRCXZImpl

// Jump short if rCX register is 0

trait JRCXZImpl {
  implicit object JRCXZ_0 extends JRCXZ._1[rel8] {
    def opcode = 0xE3
    override def hasImplicitOperand = true
  }
}
