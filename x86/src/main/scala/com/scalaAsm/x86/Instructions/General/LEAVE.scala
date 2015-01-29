package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object LEAVE extends InstructionDefinition[OneOpcode]("LEAVE") with LEAVEImpl

// High Level Procedure Exit

trait LEAVEImpl {
  implicit object LEAVE_0 extends LEAVE._0 {
    def opcode = 0xC9
    override def hasImplicitOperand = true
  }
}
