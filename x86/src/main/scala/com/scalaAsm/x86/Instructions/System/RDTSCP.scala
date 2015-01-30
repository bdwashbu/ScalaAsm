package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Read Time-Stamp Counter and Processor ID
// Category: general

object RDTSCP extends InstructionDefinition[OneOpcode]("RDTSCP") with RDTSCPImpl

trait RDTSCPImpl {
  implicit object RDTSCP_0 extends RDTSCP._0 {
    def opcode = 0x1 /+ 7
    override def hasImplicitOperand = true
  }
}
