package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Reverse Subtract
// Category: general/arith

object FSUBR extends InstructionDefinition[OneOpcode]("FSUBR") with FSUBRImpl

trait FSUBRImpl {
  implicit object FSUBR_0 extends FSUBR._1[m32] {
    def opcode = 0xD8 /+ 5
    override def hasImplicitOperand = true
  }

  implicit object FSUBR_1 extends FSUBR._1[m64] {
    def opcode = 0xDC /+ 5
    override def hasImplicitOperand = true
  }
}
