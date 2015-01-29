package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JLE extends InstructionDefinition[OneOpcode]("JLE") with JLEImpl

// Jump short if less or equal/not greater ((ZF=1) OR (SF!=OF))

trait JLEImpl {
  implicit object JLE_0 extends JLE._1[rel8] {
    def opcode = 0x7E
  }
}
