package com.scalaAsm.x86
package Instructions
package Standard

trait AND extends x86Instruction {
  val mnemonic = "AND"
}

trait AND_2[OpEn, -O1 <: Operand, -O2 <: Operand] extends TwoOperandInstruction[OpEn, O1,O2] with AND

trait ANDLow {
 
  implicit object and1 extends AND_2[RM, r32, rm32] {
      val opcode: OpcodeFormat = 0x23
  } 
}

object AND extends ANDLow {

  implicit object and2 extends AND_2[RM, r16, rm16] {
      val opcode: OpcodeFormat = 0x23
  }
  
  implicit object and3 extends AND_2[MI, rm8, imm8] {
      val opcode = 0x80 /+ 4
  }
  
  implicit object and4 extends AND_2[MI, rm16, imm16] {
      val opcode = 0x81 /+ 4
  }
  
  implicit object and5 extends AND_2[MI, rm32, imm32] {
      val opcode = 0x81 /+ 4
  }
  
  implicit object and6 extends AND_2[MI, rm16, imm8] {
      val opcode = 0x83 /+ 4
  }
  
  implicit object and7 extends AND_2[MI, rm32, imm8] {
      val opcode = 0x83 /+ 4
  }
  
  implicit object and9 extends AND_2[MR, rm16, r16] {
      val opcode: OpcodeFormat = 0x21
  }
  
  implicit object and10 extends AND_2[MR, rm32, r32] {
      val opcode: OpcodeFormat = 0x21
  }
  
  implicit object and11 extends AND_2[RM, r8, rm8] {
      val opcode: OpcodeFormat = 0x22
  }
}