package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Sine and Cosine
// Category: general/trans

object FSINCOS extends InstructionDefinition[OneOpcode]("FSINCOS") with FSINCOSImpl

trait FSINCOSImpl {
  implicit object FSINCOS_0 extends FSINCOS._0 {
    def opcode = 0xD9 /+ 7
    override def hasImplicitOperand = true
  }
}
