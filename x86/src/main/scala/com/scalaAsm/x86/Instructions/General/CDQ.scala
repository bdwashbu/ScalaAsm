package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Convert
// Category: general/conver

object CDQ extends InstructionDefinition[OneOpcode]("CDQ") with CDQImpl

trait CDQImpl {
  implicit object CDQ_0 extends CDQ._0 {
    def opcode = 0x99
    override def hasImplicitOperand = true
  }
}
