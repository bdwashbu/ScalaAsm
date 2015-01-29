package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object AAA extends InstructionDefinition[OneOpcode]("AAA") with AAAImpl

// ASCII Adjust After Addition

trait AAAImpl {
  implicit object AAA_0 extends AAA._0 {
    def opcode = 0x37
    override def hasImplicitOperand = true
  }
}
