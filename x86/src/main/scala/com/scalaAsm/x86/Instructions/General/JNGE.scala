package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JNGE extends InstructionDefinition[OneOpcode]("JNGE") with JNGEImpl

// Jump short if less/not greater (SF!=OF)

trait JNGEImpl {
  implicit object JNGE_0 extends JNGE._1[rel8] {
    def opcode = 0x7C
  }
}
