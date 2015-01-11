package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object PUSH extends InstructionDefinition[OneOpcode]("PUSH") with PUSHImpl

trait PUSHLow {
  implicit object PUSH_255_rm16 extends PUSH._1_new[rm16] {
    def opcode = 0xFF /+ 6
    override def hasImplicateOperand = true
  }

  implicit object PUSH_255_rm32 extends PUSH._1_new[rm32] {
    def opcode = 0xFF /+ 6
    override def hasImplicateOperand = true
  }

  implicit object PUSH_255_rm64 extends PUSH._1_new[rm64] {
    def opcode = 0xFF /+ 6
    override def prefix = REX.W(true)
    override def hasImplicateOperand = true
  }
}

trait PUSHImpl extends PUSHLow {
  implicit object PUSH_6_ES extends PUSH._1_new[ES] {
    def opcode = 0x6
    override def hasImplicateOperand = true
  }

  implicit object PUSH_14_CS extends PUSH._1_new[CS] {
    def opcode = 0xE
    override def hasImplicateOperand = true
  }

  implicit object PUSH_22_SS extends PUSH._1_new[SS] {
    def opcode = 0x16
    override def hasImplicateOperand = true
  }

  implicit object PUSH_30_DS extends PUSH._1_new[DS] {
    def opcode = 0x1E
    override def hasImplicateOperand = true
  }

  implicit object PUSH_80_r16 extends PUSH._1_new[r16] {
    def opcode = 0x50 + rw
    override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }

  implicit object PUSH_80_r32 extends PUSH._1_new[r32] {
    def opcode = 0x50 + rd
    override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }

  implicit object PUSH_80_r64 extends PUSH._1_new[r64] {
    def opcode = 0x50 + ro
    override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def prefix = REX.W(true)
    override def hasImplicateOperand = true
  }

  implicit object PUSH_104_imm16 extends PUSH._1_new[imm16] {
    def opcode = 0x68
    override def hasImplicateOperand = true
  }

  implicit object PUSH_104_imm32 extends PUSH._1_new[imm32] {
    def opcode = 0x68
    override def hasImplicateOperand = true
  }

  implicit object PUSH_106_imm8 extends PUSH._1_new[imm8] {
    def opcode = 0x6A
    override def hasImplicateOperand = true
  }
}
