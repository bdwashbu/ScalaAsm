package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object MOVS extends InstructionDefinition[OneOpcode]("MOVS") with MOVSImpl

// Move Data from String to String

trait MOVSImpl {
  implicit object MOVS_0 extends MOVS._0 {
    def opcode = 0xA4
    override def hasImplicitOperand = true
  }
}
