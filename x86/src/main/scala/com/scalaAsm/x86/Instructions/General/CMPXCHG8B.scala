package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object CMPXCHG8B extends InstructionDefinition[OneOpcode]("CMPXCHG8B") with CMPXCHG8BImpl

// Compare and Exchange Bytes

trait CMPXCHG8BImpl {
  implicit object CMPXCHG8B_0 extends CMPXCHG8B._1[m64] {
    def opcode = 0xC7 /+ 1
    override def hasImplicitOperand = true
  }
}
