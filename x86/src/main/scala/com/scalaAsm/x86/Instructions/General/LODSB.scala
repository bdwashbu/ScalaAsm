package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Load String
// Category: general/datamovstring

object LODSB extends InstructionDefinition[OneOpcode]("LODSB") with LODSBImpl

trait LODSBImpl {
  implicit object LODSB_0 extends LODSB._0 {
    def opcode = 0xAC
    override def hasImplicitOperand = true
  }
}
