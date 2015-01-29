package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object LODSW extends InstructionDefinition[OneOpcode]("LODSW") with LODSWImpl

// Load String

trait LODSWImpl {
  implicit object LODSW_0 extends LODSW._0 {
    def opcode = 0xAD
    override def hasImplicitOperand = true
  }
}
