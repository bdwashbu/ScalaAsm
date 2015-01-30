package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store x87 FPU Status Word
// Category: general/control

object FSTSW extends InstructionDefinition[OneOpcode]("FSTSW") with FSTSWImpl

trait FSTSWImpl {
  implicit object FSTSW_0 extends FSTSW._1[m16] {
    def opcode = 0xDD /+ 7
  }
}
