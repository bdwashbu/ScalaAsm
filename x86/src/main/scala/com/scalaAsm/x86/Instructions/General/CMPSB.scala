package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CMPSB extends InstructionDefinition[OneOpcode]("CMPSB") with CMPSBImpl

// Compare String Operands

trait CMPSBImpl {
  implicit object CMPSB_0 extends CMPSB._0 {
    def opcode = 0xA6
    override def hasImplicitOperand = true
  }
}
