package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JNG extends InstructionDefinition[OneOpcode]("JNG") with JNGImpl

// Jump short if less or equal/not greater ((ZF=1) OR (SF!=OF))

trait JNGImpl {
  implicit object JNG_0 extends JNG._1[rel8] {
    def opcode = 0x7E
  }
}
