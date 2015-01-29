package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Compare String Operands
// Category: general/arithstring/binary

object CMPSD extends InstructionDefinition[OneOpcode]("CMPSD") with CMPSDImpl

trait CMPSDImpl {
  implicit object CMPSD_0 extends CMPSD._0 {
    def opcode = 0xA7
    override def hasImplicitOperand = true
  }
}
