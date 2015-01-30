package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Load All of the CPU Registers
// Category: general/branch

object LOADALL extends InstructionDefinition[OneOpcode]("LOADALL") with LOADALLImpl

trait LOADALLImpl {
  implicit object LOADALL_0 extends LOADALL._0 {
    def opcode = 0x5
    override def hasImplicitOperand = true
  }
}
