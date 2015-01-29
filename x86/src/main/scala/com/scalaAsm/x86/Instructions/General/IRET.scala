package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object IRET extends InstructionDefinition[OneOpcode]("IRET") with IRETImpl

// Interrupt Return

trait IRETImpl {
  implicit object IRET_0 extends IRET._0 {
    def opcode = 0xCF
    override def hasImplicitOperand = true
  }
}
