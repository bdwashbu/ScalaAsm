package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Reverse Divide
// Category: general/arith

object FDIVR extends InstructionDefinition[OneOpcode]("FDIVR") with FDIVRImpl

trait FDIVRImpl {
  implicit object FDIVR_0 extends FDIVR._1[m32] {
    def opcode = 0xD8 /+ 7
    override def hasImplicitOperand = true
  }

  implicit object FDIVR_1 extends FDIVR._1[m64] {
    def opcode = 0xDC /+ 7
    override def hasImplicitOperand = true
  }
}