package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Scan String
// Category: general/arithstring/binary

object SCASW extends InstructionDefinition[OneOpcode]("SCASW") with SCASWImpl

trait SCASWImpl {
  implicit object SCASW_0 extends SCASW._0 {
    def opcode = 0xAF
    override def hasImplicitOperand = true
  }
}
