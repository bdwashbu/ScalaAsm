package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Add
// Category: general/arith

object FADD extends InstructionDefinition[OneOpcode]("FADD") with FADDImpl

trait FADDImpl {
  implicit object FADD_0 extends FADD._1[m32] {
    def opcode = 0xD8 /+ 0
    override def hasImplicitOperand = true
  }

  implicit object FADD_1 extends FADD._1[m64] {
    def opcode = 0xDC /+ 0
    override def hasImplicitOperand = true
  }
}
