package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Multiply and Pop
// Category: general/arith

object FMULP extends InstructionDefinition[OneOpcode]("FMULP") with FMULPImpl

trait FMULPImpl {
  implicit object FMULP_0 extends FMULP._0 {
    def opcode = 0xDE /+ 1
    override def hasImplicitOperand = true
  }
}
