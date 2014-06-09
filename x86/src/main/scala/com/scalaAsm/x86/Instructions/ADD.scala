package com.scalaAsm.x86
package Instructions

trait ADD extends x86Instruction {
  val mnemonic = "ADD"
}

trait ADD_2[OpEn, -O1 <: Operand, -O2 <: Operand] extends TwoOperandInstruction[OpEn, O1,O2] with ADD

object ADD {
  
  implicit object add1 extends ADD_2[MI, rm32, imm8] {
      val opcode = 0x83 /+ 0
  }
  
  implicit object add2 extends ADD_2[MI, rm16, imm8] {
      val opcode = 0x83 /+ 0
  }
  
  implicit object add3 extends ADD_2[RM, r32, rm32] {
      val opcode: OpcodeFormat = 0x03
  }
}