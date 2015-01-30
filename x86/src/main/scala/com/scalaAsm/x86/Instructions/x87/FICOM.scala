package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Compare Integer
// Category: general/compar

object FICOM extends InstructionDefinition[OneOpcode]("FICOM") with FICOMImpl

trait FICOMImpl {
  implicit object FICOM_0 extends FICOM._1[m32] {
    def opcode = 0xDA /+ 2
    override def hasImplicitOperand = true
  }

  implicit object FICOM_1 extends FICOM._1[m16] {
    def opcode = 0xDE /+ 2
    override def hasImplicitOperand = true
  }
}
