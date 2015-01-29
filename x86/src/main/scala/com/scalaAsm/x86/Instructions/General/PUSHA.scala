package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object PUSHA extends InstructionDefinition[OneOpcode]("PUSHA") with PUSHAImpl

// Push All General-Purpose Registers

trait PUSHAImpl {
  implicit object PUSHA_0 extends PUSHA._0 {
    def opcode = 0x60
    override def hasImplicitOperand = true
  }
}
