package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object SCASW extends InstructionDefinition[OneOpcode]("SCASW") with SCASWImpl

// Scan String

trait SCASWImpl {
  implicit object SCASW_0 extends SCASW._0 {
    def opcode = 0xAF
    override def hasImplicitOperand = true
  }
}
