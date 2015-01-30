package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Set Byte on Condition - not less nor equal/greater ((ZF=0) AND (SF=OF))
// Category: general/datamov

object SETG extends InstructionDefinition[OneOpcode]("SETG") with SETGImpl

trait SETGImpl {
  implicit object SETG_0 extends SETG._1[rm8] {
    def opcode = 0x9F /+ 0
  }
}
