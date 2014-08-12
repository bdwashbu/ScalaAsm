package com.scalaAsm.x86
package Instructions
package Standard

trait MOV extends x86Instruction {
  val mnemonic = "MOV"
}

trait MOV_2[OpEn, -O1, -O2] extends TwoOperandInstruction[OpEn, O1,O2] with MOV

trait MOVLow {
  
 implicit object mov1 extends MOV_2[MR, rm32, r32] {
      val opcode = OneOpcode(0x89)
  }
}

object MOV extends MOVLow {
 
  implicit object mov3 extends MOV_2[RM, r32, rm32] {
      val opcode = OneOpcode(0x8B)
  }
  
  implicit object mov9 extends MOV_2[RM, r16, rm16] {
      val opcode = OneOpcode(0x8B)
  }
  
  implicit object mov7 extends MOV_2[OI, r16, imm16] {
      val opcode = OneOpcode(0xB8) + rw
  }
  
  implicit object mov8 extends MOV_2[OI, r8, imm8] {
      val opcode = OneOpcode(0xB0) + rb
  }
  
  implicit object mov6 extends MOV_2[OI, r32, imm32] {
      val opcode = OneOpcode(0xB8) + rd
  }
   
  implicit object mov10 extends MOV_2[OI, r64, imm64] {
      val opcode = OneOpcode(0xB8) + rd
  }
  
  implicit object mov11 extends MOV_2[MR, rm64, r64] {
      val opcode = OneOpcode(0x89)
  }
}