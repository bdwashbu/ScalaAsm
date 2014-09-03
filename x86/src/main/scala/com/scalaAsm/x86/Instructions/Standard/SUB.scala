package com.scalaAsm.x86
package Instructions
package Standard

trait SUB extends x86Instruction {
  val mnemonic = "SUB"
}

trait SUB_2[-O1, -O2, OpEn[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn] with SUB

object SUB {
  
  implicit object sub1 extends SUB_2[rm32, imm8, MI] {
      val opcode = 0x83 /+ 5
  }
  
  implicit object sub3 extends SUB_2[rm64, imm8, MI] {
      val opcode = 0x83 /+ 5
  }
  
  implicit object sub2 extends SUB_2[r32, rm32, RM] {
    val opcode: OpcodeFormat = 0x2B     
  }
}