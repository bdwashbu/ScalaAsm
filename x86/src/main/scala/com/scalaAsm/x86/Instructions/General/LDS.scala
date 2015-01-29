package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object LDS extends InstructionDefinition[OneOpcode]("LDS") with LDSImpl

// Load Far Pointer

trait LDSImpl {
  implicit object LDS_0 extends LDS._2[r16, m] {
    def opcode = 0xC5 /r
    override def hasImplicitOperand = true
  }

  implicit object LDS_1 extends LDS._2[r32, m] {
    def opcode = 0xC5 /r
    override def hasImplicitOperand = true
  }
}
