package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store Integer and Pop
// Category: general/datamov

object FISTP extends InstructionDefinition[OneOpcode]("FISTP") with FISTPImpl

trait FISTPImpl {
  implicit object FISTP_0 extends FISTP._1[m32] {
    def opcode = 0xDB /+ 3
    override def hasImplicitOperand = true
  }

  implicit object FISTP_1 extends FISTP._1[m16] {
    def opcode = 0xDF /+ 3
    override def hasImplicitOperand = true
  }
}
