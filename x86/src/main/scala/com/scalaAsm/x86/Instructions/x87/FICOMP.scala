package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Compare Integer and Pop
// Category: general/compar

object FICOMP extends InstructionDefinition[OneOpcode]("FICOMP") with FICOMPImpl

trait FICOMPImpl {
  implicit object FICOMP_0 extends FICOMP._1[m32] {
    def opcode = 0xDA /+ 3
    override def hasImplicitOperand = true
  }

  implicit object FICOMP_1 extends FICOMP._1[m16] {
    def opcode = 0xDE /+ 3
    override def hasImplicitOperand = true
  }
}
