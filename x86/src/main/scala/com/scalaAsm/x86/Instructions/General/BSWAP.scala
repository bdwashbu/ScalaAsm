package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Byte Swap
// Category: general/datamov

object BSWAP extends InstructionDefinition[OneOpcode]("BSWAP") with BSWAPImpl

trait BSWAPImpl {
  implicit object BSWAP_0 extends BSWAP._1[r16] {
    def opcode = 0xC8 + rw
    override def explicitFormat(op1: r16) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }

  implicit object BSWAP_1 extends BSWAP._1[r32] {
    def opcode = 0xC8 + rd
    override def explicitFormat(op1: r32) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }

  implicit object BSWAP_2 extends BSWAP._1[r64] {
    def opcode = 0xC8 + ro
    override def prefix = REX.W(true)
    override def explicitFormat(op1: r64) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
  }
}
