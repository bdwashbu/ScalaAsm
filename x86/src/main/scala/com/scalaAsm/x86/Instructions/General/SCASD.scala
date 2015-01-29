package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object SCASD extends InstructionDefinition[OneOpcode]("SCASD") with SCASDImpl

// Scan String

trait SCASDImpl {
  implicit object SCASD_0 extends SCASD._0 {
    def opcode = 0xAF
    override def hasImplicitOperand = true
  }
}
