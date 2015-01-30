package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Multiply
// Category: general/arith

object FIMUL extends InstructionDefinition[OneOpcode]("FIMUL") with FIMULImpl

trait FIMULImpl {
  implicit object FIMUL_0 extends FIMUL._1[m32] {
    def opcode = 0xDA /+ 1
    override def hasImplicitOperand = true
  }

  implicit object FIMUL_1 extends FIMUL._1[m16] {
    def opcode = 0xDE /+ 1
    override def hasImplicitOperand = true
  }
}
