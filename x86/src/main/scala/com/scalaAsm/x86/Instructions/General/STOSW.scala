package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object STOSW extends InstructionDefinition[OneOpcode]("STOSW") with STOSWImpl

// Store String

trait STOSWImpl {
  implicit object STOSW_0 extends STOSW._0 {
    def opcode = 0xAB
    override def hasImplicitOperand = true
  }
}
