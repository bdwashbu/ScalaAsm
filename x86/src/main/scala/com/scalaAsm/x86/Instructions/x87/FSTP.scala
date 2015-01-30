package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store Floating Point Value and Pop
// Category: general/datamov

object FSTP extends InstructionDefinition[OneOpcode]("FSTP") with FSTPImpl

trait FSTPImpl {
  implicit object FSTP_0 extends FSTP._1[m32] {
    def opcode = 0xD9 /+ 3
    override def hasImplicitOperand = true
  }

  implicit object FSTP_1 extends FSTP._1[m64] {
    def opcode = 0xDD /+ 3
    override def hasImplicitOperand = true
  }
}
