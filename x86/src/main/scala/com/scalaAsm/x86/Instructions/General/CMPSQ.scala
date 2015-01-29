package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Compare String Operands
// Category: general/arithstring/binary

object CMPSQ extends InstructionDefinition[OneOpcode]("CMPSQ") with CMPSQImpl

trait CMPSQImpl {
  implicit object CMPSQ_0 extends CMPSQ._0 {
    def opcode = 0xA7
    override def hasImplicitOperand = true
  }
}
