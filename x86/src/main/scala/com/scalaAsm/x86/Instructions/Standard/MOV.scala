package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.TwoOperandFormat

trait MOVInstruction extends x86Instruction {
  val mnemonic = "MOV"
}

trait Mov2Def[OpEn, -O1, -O2] extends TwoOperandInstruction[OpEn, O1,O2] with MOVInstruction

trait MOVLow {
  
 implicit object mov1 extends Mov2Def[MR, rm32, r32] {
      val opcode = OneOpcode(0x89)
  }
}

object MOVInstruction extends MOVLow {
 
  implicit object mov3 extends Mov2Def[RM, r32, rm32] {
      val opcode = OneOpcode(0x8B)
  }
  
  implicit object mov9 extends Mov2Def[RM, r16, rm16] {
      val opcode = OneOpcode(0x8B)
  }
  
  implicit object mov7 extends Mov2Def[OI, r16, imm16] {
      val opcode = OneOpcode(0xB8) + rw
  }
  
  implicit object mov8 extends Mov2Def[OI, r8, imm8] {
      val opcode = OneOpcode(0xB0) + rb
  }
  
  implicit object mov6 extends Mov2Def[OI, r32, imm32] {
      val opcode = OneOpcode(0xB8) + rd
  }
   
  implicit object mov10 extends Mov2Def[OI, r64, imm64] {
      val opcode = OneOpcode(0xB8) + rd
  }
  
  implicit object mov11 extends Mov2Def[MR, rm64, r64] {
      val opcode = OneOpcode(0x89)
  }
  
  implicit object mov12 extends Mov2Def[MI, rm64, imm32] {
      val opcode = 0xC7 /+ 0
  }
}