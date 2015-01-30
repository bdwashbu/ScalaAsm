package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Verify a Segment for Reading
// Category: general

object VERR extends InstructionDefinition[OneOpcode]("VERR") with VERRImpl

trait VERRImpl {
  implicit object VERR_0 extends VERR._1[rm16] {
    def opcode = 0x0 /+ 4
  }
}
