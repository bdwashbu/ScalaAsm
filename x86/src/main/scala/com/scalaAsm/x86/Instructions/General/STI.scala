package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object STI extends InstructionDefinition[OneOpcode]("STI") with STIImpl

// Set Interrupt Flag

trait STIImpl {
  implicit object STI_0 extends STI._0 {
    def opcode = 0xFB
  }
}
