package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JCXZ extends InstructionDefinition[OneOpcode]("JCXZ") with JCXZImpl

// Jump short if eCX register is 0

trait JCXZImpl {
  implicit object JCXZ_0 extends JCXZ._1[rel8] {
    def opcode = 0xE3
    override def hasImplicitOperand = true
  }
}
