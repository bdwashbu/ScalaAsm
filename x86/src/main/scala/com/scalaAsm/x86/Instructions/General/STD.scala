package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object STD extends InstructionDefinition[OneOpcode]("STD") with STDImpl

// Set Direction Flag

trait STDImpl {
  implicit object STD_0 extends STD._0 {
    def opcode = 0xFD
  }
}
