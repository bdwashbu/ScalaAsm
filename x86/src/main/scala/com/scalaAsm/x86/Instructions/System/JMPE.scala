package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump to IA-64 Instruction Set
// Category: general/branch

object JMPE extends InstructionDefinition[OneOpcode]("JMPE") with JMPEImpl

trait JMPEImpl {
  implicit object JMPE_0 extends JMPE._0 {
    def opcode = 0x0 /+ 6
  }
}
