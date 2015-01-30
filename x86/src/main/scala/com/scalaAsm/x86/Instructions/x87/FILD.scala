package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Load Integer
// Category: general/datamov

object FILD extends InstructionDefinition[OneOpcode]("FILD") with FILDImpl

trait FILDImpl {
  implicit object FILD_0 extends FILD._1[m32] {
    def opcode = 0xDB /+ 0
    override def hasImplicitOperand = true
  }

  implicit object FILD_1 extends FILD._1[m16] {
    def opcode = 0xDF /+ 0
    override def hasImplicitOperand = true
  }
}
