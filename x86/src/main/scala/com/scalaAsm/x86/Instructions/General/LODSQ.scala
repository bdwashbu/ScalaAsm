package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Load String
// Category: general/datamovstring

object LODSQ extends InstructionDefinition[OneOpcode]("LODSQ") with LODSQImpl

trait LODSQImpl {
  implicit object LODSQ_0 extends LODSQ._0 {
    def opcode = 0xAD
    override def hasImplicitOperand = true
  }
}
