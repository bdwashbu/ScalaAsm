package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object INC extends InstructionDefinition[OneOpcode]("INC") with INCImpl

// Increment by 1

trait INCLow {
  implicit object INC_0 extends INC._1[rm16] {
    def opcode = 0xFF /+ 0
  }

  implicit object INC_1 extends INC._1[rm32] {
    def opcode = 0xFF /+ 0
  }

  implicit object INC_2 extends INC._1[rm64] {
    def opcode = 0xFF /+ 0
    override def prefix = REX.W(true)
  }
}

trait INCImpl extends INCLow {
  implicit object INC_3 extends INC._1[r16] {
    def opcode = 0x40 + rw
    override def explicitFormat(op1: r16) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }

  implicit object INC_4 extends INC._1[r32] {
    def opcode = 0x40 + rd
    override def explicitFormat(op1: r32) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }

  implicit object INC_5 extends INC._1[rm8] {
    def opcode = 0xFE /+ 0
  }
}
