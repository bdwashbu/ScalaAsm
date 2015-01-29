package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CLI extends InstructionDefinition[OneOpcode]("CLI") with CLIImpl

// Clear Interrupt Flag

trait CLIImpl {
  implicit object CLI_0 extends CLI._0 {
    def opcode = 0xFA
  }
}
