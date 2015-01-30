package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Multiply
// Category: general/arith

object FMUL extends InstructionDefinition[OneOpcode]("FMUL") with FMULImpl

trait FMULImpl {
  implicit object FMUL_0 extends FMUL._1[m32] {
    def opcode = 0xD8 /+ 1
    override def hasImplicitOperand = true
  }

  implicit object FMUL_1 extends FMUL._1[m64] {
    def opcode = 0xDC /+ 1
    override def hasImplicitOperand = true
  }
}
