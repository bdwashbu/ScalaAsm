package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JNBE extends InstructionDefinition[OneOpcode]("JNBE") with JNBEImpl

// Jump short if not below or equal/above (CF=0 AND ZF=0)

trait JNBEImpl {
  implicit object JNBE_0 extends JNBE._1[rel8] {
    def opcode = 0x77
  }
}
