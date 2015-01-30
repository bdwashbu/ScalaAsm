package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Divide
// Category: general/arith

object FDIV extends InstructionDefinition[OneOpcode]("FDIV") with FDIVImpl

trait FDIVImpl {
  implicit object FDIV_0 extends FDIV._1[m32] {
    def opcode = 0xD8 /+ 6
    override def hasImplicitOperand = true
  }

  implicit object FDIV_1 extends FDIV._1[m64] {
    def opcode = 0xDC /+ 6
    override def hasImplicitOperand = true
  }
}
