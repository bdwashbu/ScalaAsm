package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object PUSH extends InstructionDefinition[OneOpcode]("PUSH") with PUSHImpl

// Push Word, Doubleword or Quadword Onto the Stack

trait PUSHLow {
  implicit object PUSH_0 extends PUSH._1[rm16] {
    def opcode = 0xFF /+ 6
    override def hasImplicateOperand = true
  }

  implicit object PUSH_1 extends PUSH._1[rm32] {
    def opcode = 0xFF /+ 6
    override def hasImplicateOperand = true
  }

  implicit object PUSH_2 extends PUSH._1[rm64] {
    def opcode = 0xFF /+ 6
    override def hasImplicateOperand = true
  }
}

trait PUSHImpl extends PUSHLow {
  implicit object PUSH_3 extends PUSH._1[ES] {
    def opcode = 0x6
    override def hasImplicateOperand = true
  }

  implicit object PUSH_4 extends PUSH._1[CS] {
    def opcode = 0xE
    override def hasImplicateOperand = true
  }

  implicit object PUSH_5 extends PUSH._1[SS] {
    def opcode = 0x16
    override def hasImplicateOperand = true
  }

  implicit object PUSH_6 extends PUSH._1[DS] {
    def opcode = 0x1E
    override def hasImplicateOperand = true
  }

  implicit object PUSH_7 extends PUSH._1[r16] {
    def opcode = 0x50 + rw
    override def explicitFormat(op1: r16) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }

  implicit object PUSH_8 extends PUSH._1[r32] {
    def opcode = 0x50 + rd
    override def explicitFormat(op1: r32) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }

  implicit object PUSH_9 extends PUSH._1[r64] {
    def opcode = 0x50 + ro
    override def explicitFormat(op1: r64) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }

  implicit object PUSH_10 extends PUSH._1[imm16] {
    def opcode = 0x68
    override def hasImplicateOperand = true
  }

  implicit object PUSH_11 extends PUSH._1[imm32] {
    def opcode = 0x68
    override def hasImplicateOperand = true
  }

  implicit object PUSH_12 extends PUSH._1[imm8] {
    def opcode = 0x6A
    override def hasImplicateOperand = true
  }
}
