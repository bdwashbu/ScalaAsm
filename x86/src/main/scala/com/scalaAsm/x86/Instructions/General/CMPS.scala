package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Compare String Operands
// Category: general/arithstring/binary

object CMPS extends InstructionDefinition[OneOpcode]("CMPS") with CMPSImpl

trait CMPSImpl {
  implicit object CMPS_0 extends CMPS._0 {
    def opcode = 0xA6
    override def hasImplicitOperand = true
  }
}
