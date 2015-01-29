package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CMPS extends InstructionDefinition[OneOpcode]("CMPS") with CMPSImpl

// Compare String Operands

trait CMPSImpl {
  implicit object CMPS_0 extends CMPS._0 {
    def opcode = 0xA6
    override def hasImplicitOperand = true
  }
}
