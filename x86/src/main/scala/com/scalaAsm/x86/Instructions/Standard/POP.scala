package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object POP extends InstructionDefinition[OneOpcode]("POP") with POPImpl

// Pop a Value from the Stack

trait POPLow {
  implicit object POP_0 extends POP._1[rm16] {
    def opcode = 0x8F /+ 0
    override def hasImplicateOperand = true
  }

  implicit object POP_1 extends POP._1[rm32] {
    def opcode = 0x8F /+ 0
    override def hasImplicateOperand = true
  }

  implicit object POP_2 extends POP._1[rm64] {
    def opcode = 0x8F /+ 0
    override def hasImplicateOperand = true
  }
}

trait POPImpl extends POPLow {
  implicit object POP_3 extends POP._1[ES] {
    def opcode = 0x7
    override def hasImplicateOperand = true
  }

  implicit object POP_4 extends POP._1[SS] {
    def opcode = 0x17
    override def hasImplicateOperand = true
  }

  implicit object POP_5 extends POP._1[DS] {
    def opcode = 0x1F
    override def hasImplicateOperand = true
  }

  implicit object POP_6 extends POP._1[r16] {
    def opcode = 0x58 + rw
    override def explicitFormat(op1: r16) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }

  implicit object POP_7 extends POP._1[r32] {
    def opcode = 0x58 + rd
    override def explicitFormat(op1: r32) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }

  implicit object POP_8 extends POP._1[r64] {
    def opcode = 0x58 + ro
    override def explicitFormat(op1: r64) = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }
}
