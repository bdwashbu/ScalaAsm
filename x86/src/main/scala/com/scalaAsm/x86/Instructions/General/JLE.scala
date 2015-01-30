package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if less or equal/not greater ((ZF=1) OR (SF!=OF))
// Category: general/branch/cond

object JLE extends InstructionDefinition[OneOpcode]("JLE") with JLEImpl

trait JLEImpl {
  implicit object JLE_0 extends JLE._1[rel8] {
    def opcode = 0x7E
  }

  implicit object JLE_1 extends JLE._1[rel16] {
    def opcode = 0x8E
  }

  implicit object JLE_2 extends JLE._1[rel32] {
    def opcode = 0x8E
  }
}
