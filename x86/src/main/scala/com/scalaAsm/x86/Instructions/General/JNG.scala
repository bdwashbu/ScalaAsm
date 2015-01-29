package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Jump short if less or equal/not greater ((ZF=1) OR (SF!=OF))
// Category: general/branch/cond

object JNG extends InstructionDefinition[OneOpcode]("JNG") with JNGImpl

trait JNGImpl {
  implicit object JNG_0 extends JNG._1[rel8] {
    def opcode = 0x7E
  }
}
