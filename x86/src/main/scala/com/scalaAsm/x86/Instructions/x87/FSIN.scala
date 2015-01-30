package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Sine
// Category: general/trans

object FSIN extends InstructionDefinition[OneOpcode]("FSIN") with FSINImpl

trait FSINImpl {
  implicit object FSIN_0 extends FSIN._0 {
    def opcode = 0xD9 /+ 7
    override def hasImplicitOperand = true
  }
}
