package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Divide and Pop
// Category: general/arith

object FDIVP extends InstructionDefinition[OneOpcode]("FDIVP") with FDIVPImpl

trait FDIVPImpl {
  implicit object FDIVP_0 extends FDIVP._0 {
    def opcode = 0xDE /+ 7
    override def hasImplicitOperand = true
  }
}
