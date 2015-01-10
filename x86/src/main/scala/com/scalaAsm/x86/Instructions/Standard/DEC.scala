package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object DEC extends InstructionDefinition[OneOpcode]("DEC") with DECImpl

trait DECLow {
  implicit object DEC_255_rm16 extends DEC._1_new[rm16] {
    def opcode = 0xFF /+ 1
  }

  implicit object DEC_255_rm32 extends DEC._1_new[rm32] {
    def opcode = 0xFF /+ 1
  }

  implicit object DEC_255_rm64 extends DEC._1_new[rm64] {
    def opcode = 0xFF /+ 1
    override def prefix = REX.W(true)
  }
}

trait DECImpl extends DECLow {
  implicit object DEC_72_r16 extends DEC._1_new[r16] {
    def opcode = 0x48 + rw
    override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }

  implicit object DEC_72_r32 extends DEC._1_new[r32] {
    def opcode = 0x48 + rd
    override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }

  implicit object DEC_254_rm8 extends DEC._1_new[rm8] {
    def opcode = 0xFE /+ 1
  }
}
