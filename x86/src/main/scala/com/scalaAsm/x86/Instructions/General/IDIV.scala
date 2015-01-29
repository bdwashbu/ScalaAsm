package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object IDIV extends InstructionDefinition[OneOpcode]("IDIV") with IDIVImpl

// Signed Divide

trait IDIVLow {
  implicit object IDIV_0 extends IDIV._1[rm16] {
    def opcode = 0xF7 /+ 7
    override def hasImplicitOperand = true
  }

  implicit object IDIV_1 extends IDIV._1[rm32] {
    def opcode = 0xF7 /+ 7
    override def hasImplicitOperand = true
  }

  implicit object IDIV_2 extends IDIV._1[rm64] {
    def opcode = 0xF7 /+ 7
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}

trait IDIVImpl extends IDIVLow {
  implicit object IDIV_3 extends IDIV._1[rm8] {
    def opcode = 0xF6 /+ 7
    override def hasImplicitOperand = true
  }
}
