package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.TwoOperandFormat

trait MOVInstruction extends x86Instruction {
  val mnemonic = "MOV"
}

trait Mov2Def[-O1, -O2, OpEn[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn] with MOVInstruction

trait MOVLow {
  
 implicit object mov1 extends Mov2Def[rm32, r32, MR] {
      val opcode = OneOpcode(0x89)
  }
}

object MOVInstruction extends MOVLow {
 
  implicit object mov3 extends Mov2Def[r32, rm32, RM] {
      val opcode = OneOpcode(0x8B)
  }
  
  implicit object mov9 extends Mov2Def[r16, rm16, RM] {
      val opcode = OneOpcode(0x8B)
  }
  
  implicit object mov7 extends Mov2Def[r16, imm16, OI] {
      val opcode = OneOpcode(0xB8) + rw
  }
  
  implicit object mov8 extends Mov2Def[r8, imm8, OI] {
      val opcode = OneOpcode(0xB0) + rb
  }
  
  implicit object mov6 extends Mov2Def[r32, imm32, OI] {
      val opcode = OneOpcode(0xB8) + rd
  }
   
  implicit object mov10 extends Mov2Def[r64, imm64, OI] {
      val opcode = OneOpcode(0xB8) + rd
  }
  
  implicit object mov11 extends Mov2Def[rm64, r64, MR] {
      val opcode = OneOpcode(0x89)
  }
  
  implicit object mov12 extends Mov2Def[rm64, imm32, MI] {
      val opcode = 0xC7 /+ 0
  }
}