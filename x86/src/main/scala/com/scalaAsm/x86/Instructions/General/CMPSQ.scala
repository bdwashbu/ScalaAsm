package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CMPSQ extends InstructionDefinition[OneOpcode]("CMPSQ") with CMPSQImpl

// Compare String Operands

trait CMPSQImpl {
  implicit object CMPSQ_0 extends CMPSQ._0 {
    def opcode = 0xA7
    override def hasImplicitOperand = true
  }
}
