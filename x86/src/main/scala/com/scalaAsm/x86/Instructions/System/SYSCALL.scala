package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Fast System Call
// Category: general/branch

object SYSCALL extends InstructionDefinition[OneOpcode]("SYSCALL") with SYSCALLImpl

trait SYSCALLImpl {
  implicit object SYSCALL_0 extends SYSCALL._0 {
    def opcode = 0x5
    override def hasImplicitOperand = true
  }
}
