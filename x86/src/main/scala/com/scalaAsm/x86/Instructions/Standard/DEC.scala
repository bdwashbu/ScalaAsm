package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object DEC extends InstructionDefinition[OneOpcode]("DEC") with DECImpl

// Decrement by 1

trait DECLow {
  implicit object DEC_0 extends DEC._1[rm16] {
    def opcode = 0xFF /+ 1
  }

  implicit object DEC_1 extends DEC._1[rm32] {
    def opcode = 0xFF /+ 1
  }

  implicit object DEC_2 extends DEC._1[rm64] {
    def opcode = 0xFF /+ 1
    override def prefix = REX.W(true)
  }
}

trait DECImpl extends DECLow {
  implicit object DEC_3 extends DEC._1[r16] {
    def opcode = 0x48 + rw
    override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }

  implicit object DEC_4 extends DEC._1[r32] {
    def opcode = 0x48 + rd
    override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }

  implicit object DEC_5 extends DEC._1[rm8] {
    def opcode = 0xFE /+ 1
  }
}
