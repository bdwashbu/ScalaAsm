package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if not less nor equal/greater ((ZF=0) AND (SF=OF))
// Category: general/branch/cond

object JG extends InstructionDefinition[OneOpcode]("JG") with JGImpl

trait JGImpl {
  implicit object JG_0 extends JG._1[rel8] {
    def opcode = 0x7F
  }
}
