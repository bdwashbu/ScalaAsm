package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object BOUND extends InstructionDefinition[OneOpcode]("BOUND") with BOUNDImpl

// Check Array Index Against Bounds

trait BOUNDImpl {
  implicit object BOUND_0 extends BOUND._2[r16, m16] {
    def opcode = 0x62 /r
    override def hasImplicitOperand = true
  }

  implicit object BOUND_1 extends BOUND._2[r32, m32] {
    def opcode = 0x62 /r
    override def hasImplicitOperand = true
  }
}
