package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store Integer
// Category: general/datamov

object FIST extends InstructionDefinition[OneOpcode]("FIST") with FISTImpl

trait FISTImpl {
  implicit object FIST_0 extends FIST._1[m32] {
    def opcode = 0xDB /+ 2
    override def hasImplicitOperand = true
  }

  implicit object FIST_1 extends FIST._1[m16] {
    def opcode = 0xDF /+ 2
    override def hasImplicitOperand = true
  }
}
