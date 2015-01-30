package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Subtract
// Category: general/arith

object FISUB extends InstructionDefinition[OneOpcode]("FISUB") with FISUBImpl

trait FISUBImpl {
  implicit object FISUB_0 extends FISUB._1[m32] {
    def opcode = 0xDA /+ 4
    override def hasImplicitOperand = true
  }

  implicit object FISUB_1 extends FISUB._1[m16] {
    def opcode = 0xDE /+ 4
    override def hasImplicitOperand = true
  }
}
