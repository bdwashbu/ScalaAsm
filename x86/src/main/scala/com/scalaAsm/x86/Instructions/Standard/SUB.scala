package com.scalaAsm.x86
package Instructions
package Standard

trait SUB extends x86Instruction {
  val mnemonic = "SUB"
}

trait SUB_2[OpEn, -O1 <: Operand, -O2 <: Operand] extends TwoOperandInstruction[OpEn, O1,O2] with SUB

object SUB {
  
  implicit object sub1 extends SUB_2[MI, rm32, imm8] {
      val opcode = 0x83 /+ 5
  }
  
  implicit object sub3 extends SUB_2[MI, rm64, imm8] {
      val opcode = 0x83 /+ 5
  }
  
  implicit object sub2 extends SUB_2[RM, r32, rm32] {
    val opcode: OpcodeFormat = 0x2B     
  }
}