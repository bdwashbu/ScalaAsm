package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object DAA extends InstructionDefinition[OneOpcode]("DAA") with DAAImpl

// Decimal Adjust AL after Addition

trait DAAImpl {
  implicit object DAA_0 extends DAA._0 {
    def opcode = 0x27
    override def hasImplicitOperand = true
  }
}
