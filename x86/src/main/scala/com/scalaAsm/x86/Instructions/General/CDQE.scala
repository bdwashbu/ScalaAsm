package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CDQE extends InstructionDefinition[OneOpcode]("CDQE") with CDQEImpl

// Convert

trait CDQEImpl {
  implicit object CDQE_0 extends CDQE._0 {
    def opcode = 0x98
    override def hasImplicitOperand = true
  }
}
