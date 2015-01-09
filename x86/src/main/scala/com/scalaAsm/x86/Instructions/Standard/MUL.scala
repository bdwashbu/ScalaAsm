package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object MUL extends InstructionDefinition[OneOpcode]("MUL") with MULImpl

trait MULImpl {
  implicit object MUL_246_rm8 extends MUL._1_new[rm8] {
    def opcode = 0xF6 /+ 4
  }

  implicit object MUL_247_rm16 extends MUL._1_new[rm16] {
    def opcode = 0xF7 /+ 4
  }

  implicit object MUL_247_rm32 extends MUL._1_new[rm32] {
    def opcode = 0xF7 /+ 4
  }

  implicit object MUL_247_rm64 extends MUL._1_new[rm64] {
    def opcode = 0xF7 /+ 4
    override def prefix = REX.W(true)
  }
}
