package com.scalaAsm.x86
package Instructions
package Standard

trait ADD extends x86Instruction {
  val mnemonic = "ADD"
}

trait ADD_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1,O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode] with ADD

object ADD {
  
  implicit object add1 extends ADD_2[rm32, imm8, MI] {
      def opcode = 0x83 /+ 0
  }
  
  implicit object add2 extends ADD_2[rm16, imm8, MI] {
      def opcode = 0x83 /+ 0
  }
  
  implicit object add3 extends ADD_2[r32, rm32, RM] {
      def opcode = 0x03
  }
}