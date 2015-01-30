package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Subtract
// Category: general/arith

object FSUB extends InstructionDefinition[OneOpcode]("FSUB") with FSUBImpl

trait FSUBImpl {
  implicit object FSUB_0 extends FSUB._1[m32] {
    def opcode = 0xD8 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object FSUB_1 extends FSUB._1[m64] {
    def opcode = 0xDC /+ 4
    override def hasImplicitOperand = true
  }
}
