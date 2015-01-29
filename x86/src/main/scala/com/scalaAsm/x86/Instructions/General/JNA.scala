package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JNA extends InstructionDefinition[OneOpcode]("JNA") with JNAImpl

// Jump short if below or equal/not above (CF=1 AND ZF=1)

trait JNAImpl {
  implicit object JNA_0 extends JNA._1[rel8] {
    def opcode = 0x76
  }
}
