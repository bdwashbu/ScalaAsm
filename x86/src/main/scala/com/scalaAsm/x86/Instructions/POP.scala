package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._

trait POP extends x86Instruction {
  val mnemonic = "POP"
}

trait POP_1[OpEn, -O1 <: Operand] extends OneOperandInstruction[OpEn, O1] with POP

object POP {
  
  implicit object pop1 extends POP_1[O, r32] {
      val opcode = OpcodePlusRd(0x58)
  }
  
  implicit object pop3 extends POP_1[O, r64] {
      val opcode = OpcodePlusRd(0x58)
  }
  
  implicit object pop2 extends POP_1[DS, DS] {
      val opcode = OneOpcode(0x1F)
  }
}