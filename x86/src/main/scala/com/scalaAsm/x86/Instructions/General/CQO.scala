package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Convert
// Category: general/conver

object CQO extends InstructionDefinition[OneOpcode]("CQO") with CQOImpl

trait CQOImpl {
  implicit object CQO_0 extends CQO._0 {
    def opcode = 0x99
    override def hasImplicitOperand = true
  }
}
