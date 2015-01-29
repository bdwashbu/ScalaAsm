package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object POPFQ extends InstructionDefinition[OneOpcode]("POPFQ") with POPFQImpl

// Pop Stack into rFLAGS Register

trait POPFQImpl {
  implicit object POPFQ_0 extends POPFQ._0 {
    def opcode = 0x9D
    override def hasImplicitOperand = true
  }
}
