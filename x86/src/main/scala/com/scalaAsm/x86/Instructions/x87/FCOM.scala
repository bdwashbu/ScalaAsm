package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Compare Real
// Category: general/compar

object FCOM extends InstructionDefinition[OneOpcode]("FCOM") with FCOMImpl

trait FCOMImpl {
//  implicit object FCOM_0 extends FCOM._1[STi/m32] {
//    def opcode = 0xD8 /+ 2
//    override def hasImplicitOperand = true
//  }

  implicit object FCOM_1 extends FCOM._0 {
    def opcode = 0xD8 /+ 2
    override def hasImplicitOperand = true
  }

  implicit object FCOM_2 extends FCOM._1[m64] {
    def opcode = 0xDC /+ 2
    override def hasImplicitOperand = true
  }
}
