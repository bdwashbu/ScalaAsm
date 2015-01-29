package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Compare String Operands
// Category: general/arithstring/binary

object CMPSW extends InstructionDefinition[OneOpcode]("CMPSW") with CMPSWImpl

trait CMPSWImpl {
  implicit object CMPSW_0 extends CMPSW._0 {
    def opcode = 0xA7
    override def hasImplicitOperand = true
  }
}
