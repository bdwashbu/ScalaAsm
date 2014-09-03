package com.scalaAsm.x86
package Instructions
package Standard

trait ADD extends x86Instruction {
  val mnemonic = "ADD"
}

trait ADD_2[-O1, -O2, OpEn[O1,O2]] extends TwoOperandInstruction[O1, O2, OpEn] with ADD

object ADD {
  
  implicit object add1 extends ADD_2[rm32, imm8, MI] {
      val opcode = 0x83 /+ 0
  }
  
  implicit object add2 extends ADD_2[rm16, imm8, MI] {
      val opcode = 0x83 /+ 0
  }
  
  implicit object add3 extends ADD_2[r32, rm32, RM] {
      val opcode: OpcodeFormat = 0x03
  }
}