package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object IRETQ extends InstructionDefinition[OneOpcode]("IRETQ") with IRETQImpl

// Interrupt Return

trait IRETQImpl {
  implicit object IRETQ_0 extends IRETQ._0 {
    def opcode = 0xCF
    override def hasImplicitOperand = true
  }
}
