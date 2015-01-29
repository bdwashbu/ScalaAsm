package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Scan String
// Category: general/arithstring/binary

object SCAS extends InstructionDefinition[OneOpcode]("SCAS") with SCASImpl

trait SCASImpl {
  implicit object SCAS_0 extends SCAS._0 {
    def opcode = 0xAE
    override def hasImplicitOperand = true
  }
}
