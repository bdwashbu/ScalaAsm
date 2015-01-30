package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Round to Integer
// Category: general/arith

object FRNDINT extends InstructionDefinition[OneOpcode]("FRNDINT") with FRNDINTImpl

trait FRNDINTImpl {
  implicit object FRNDINT_0 extends FRNDINT._0 {
    def opcode = 0xD9 /+ 7
    override def hasImplicitOperand = true
  }
}
