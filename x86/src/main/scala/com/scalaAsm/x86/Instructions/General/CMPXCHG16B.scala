package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CMPXCHG16B extends InstructionDefinition[OneOpcode]("CMPXCHG16B") with CMPXCHG16BImpl

// Compare and Exchange Bytes

trait CMPXCHG16BImpl {
  implicit object CMPXCHG16B_0 extends CMPXCHG16B._1[m128] {
    def opcode = 0xC7 /+ 1
    override def hasImplicitOperand = true
  }
}
