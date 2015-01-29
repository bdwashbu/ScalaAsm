package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Unsigned Divide
// Category: general/arith/binary

object DIV extends InstructionDefinition[OneOpcode]("DIV") with DIVImpl

trait DIVImpl {
  implicit object DIV_0 extends DIV._1[rm8] {
    def opcode = 0xF6 /+ 6
    override def hasImplicitOperand = true
  }

  implicit object DIV_1 extends DIV._1[rm16] {
    def opcode = 0xF7 /+ 6
    override def hasImplicitOperand = true
  }

  implicit object DIV_2 extends DIV._1[rm32] {
    def opcode = 0xF7 /+ 6
    override def hasImplicitOperand = true
  }

  implicit object DIV_3 extends DIV._1[rm64] {
    def opcode = 0xF7 /+ 6
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}
