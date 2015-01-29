package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object DAS extends InstructionDefinition[OneOpcode]("DAS") with DASImpl

// Decimal Adjust AL after Subtraction

trait DASImpl {
  implicit object DAS_0 extends DAS._0 {
    def opcode = 0x2F
    override def hasImplicitOperand = true
  }
}
