package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object SAHF extends InstructionDefinition[OneOpcode]("SAHF") with SAHFImpl

// Store AH into Flags

trait SAHFImpl {
  implicit object SAHF_0 extends SAHF._0 {
    def opcode = 0x9E
    override def hasImplicitOperand = true
  }
}
