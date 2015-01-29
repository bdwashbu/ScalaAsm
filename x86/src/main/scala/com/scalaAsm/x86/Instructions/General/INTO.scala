package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object INTO extends InstructionDefinition[OneOpcode]("INTO") with INTOImpl

// Call to Interrupt Procedure

trait INTOImpl {
  implicit object INTO_0 extends INTO._0 {
    def opcode = 0xCE
    override def hasImplicitOperand = true
  }
}
