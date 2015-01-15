package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object LEAVE extends InstructionDefinition[OneOpcode]("LEAVE") with LEAVEImpl

trait LEAVEImpl {
  implicit object LEAVE_201 extends LEAVE._0 {
    def opcode = 0xC9
    override def hasImplicateOperand = true
  }
}
