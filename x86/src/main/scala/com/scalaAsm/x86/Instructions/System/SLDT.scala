package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store Local Descriptor Table Register
// Category: general

object SLDT extends InstructionDefinition[OneOpcode]("SLDT") with SLDTImpl

trait SLDTImpl {
  implicit object SLDT_0 extends SLDT._1[m16] {
    def opcode = 0x0 /+ 0
    override def hasImplicitOperand = true
  }

  implicit object SLDT_1 extends SLDT._1[r16] {
    def opcode = 0x0 /+ 0
    override def hasImplicitOperand = true
  }

  implicit object SLDT_2 extends SLDT._1[r32] {
    def opcode = 0x0 /+ 0
    override def hasImplicitOperand = true
  }

  implicit object SLDT_3 extends SLDT._1[r64] {
    def opcode = 0x0 /+ 0
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}
