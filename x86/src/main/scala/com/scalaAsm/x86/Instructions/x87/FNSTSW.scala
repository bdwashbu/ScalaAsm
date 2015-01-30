package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store x87 FPU Status Word
// Category: general/control

object FNSTSW extends InstructionDefinition[OneOpcode]("FNSTSW") with FNSTSWImpl

trait FNSTSWImpl {
  implicit object FNSTSW_0 extends FNSTSW._1[m16] {
    def opcode = 0xDD /+ 7
  }
}
