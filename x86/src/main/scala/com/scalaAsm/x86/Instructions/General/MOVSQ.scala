package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object MOVSQ extends InstructionDefinition[OneOpcode]("MOVSQ") with MOVSQImpl

// Move Data from String to String

trait MOVSQImpl {
  implicit object MOVSQ_0 extends MOVSQ._0 {
    def opcode = 0xA5
    override def hasImplicitOperand = true
  }
}
