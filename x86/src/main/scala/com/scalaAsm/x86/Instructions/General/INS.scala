package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Input from Port to String
// Category: general/inoutstring

object INS extends InstructionDefinition[OneOpcode]("INS") with INSImpl

trait INSImpl {
  implicit object INS_0 extends INS._0 {
    def opcode = 0x6C
    override def hasImplicitOperand = true
  }
}
