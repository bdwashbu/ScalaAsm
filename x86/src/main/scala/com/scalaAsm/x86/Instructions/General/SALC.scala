package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set AL If Carry
// Category: general/datamov

object SALC extends InstructionDefinition[OneOpcode]("SALC") with SALCImpl

trait SALCImpl {
  implicit object SALC_0 extends SALC._0 {
    def opcode = 0xD6
    override def hasImplicitOperand = true
  }
}
