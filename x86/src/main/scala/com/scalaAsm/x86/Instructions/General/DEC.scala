package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Decrement by 1
// Category: general/arith/binary

object DEC extends InstructionDefinition[OneOpcode]("DEC") with DECImpl

trait DECLow {
  implicit object DEC_0 extends DEC._1[rm8] {
    def opcode = 0xFE /+ 1
  }

  implicit object DEC_1 extends DEC._1[rm16] {
    def opcode = 0xFF /+ 1
  }

  implicit object DEC_2 extends DEC._1[rm32] {
    def opcode = 0xFF /+ 1
  }

  implicit object DEC_3 extends DEC._1[rm64] {
    def opcode = 0xFF /+ 1
    override def prefix = REX.W(true)
  }
}

trait DECImpl extends DECLow {
  implicit object DEC_4 extends DEC._1[r16] {
    def opcode = 0x48 + rw
    override def explicitFormat(op1: r16) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }

  implicit object DEC_5 extends DEC._1[r32] {
    def opcode = 0x48 + rd
    override def explicitFormat(op1: r32) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }
}
