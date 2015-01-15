package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object POP extends InstructionDefinition[OneOpcode]("POP") with POPImpl

trait POPLow {
  implicit object POP_143_rm16 extends POP._1[rm16] {
    def opcode = 0x8F /+ 0
    override def hasImplicateOperand = true
  }

  implicit object POP_143_rm32 extends POP._1[rm32] {
    def opcode = 0x8F /+ 0
    override def hasImplicateOperand = true
  }

  implicit object POP_143_rm64 extends POP._1[rm64] {
    def opcode = 0x8F /+ 0
    override def hasImplicateOperand = true
  }
}

trait POPImpl extends POPLow {
  implicit object POP_7_ES extends POP._1[ES] {
    def opcode = 0x7
    override def hasImplicateOperand = true
  }

  implicit object POP_23_SS extends POP._1[SS] {
    def opcode = 0x17
    override def hasImplicateOperand = true
  }

  implicit object POP_31_DS extends POP._1[DS] {
    def opcode = 0x1F
    override def hasImplicateOperand = true
  }

  implicit object POP_88_r16 extends POP._1[r16] {
    def opcode = 0x58 + rw
    override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }

  implicit object POP_88_r32 extends POP._1[r32] {
    def opcode = 0x58 + rd
    override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }

  implicit object POP_88_r64 extends POP._1[r64] {
    def opcode = 0x58 + ro
    override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))
    override def hasImplicateOperand = true
  }
}
