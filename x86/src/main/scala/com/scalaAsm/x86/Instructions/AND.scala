package com.scalaAsm.x86
package Instructions

abstract class AND extends x86Instruction("AND")

trait AND_2[-O1, -O2] extends AND with TwoOperandInstruction[O1,O2]

trait ANDLow {
 
  implicit object and1 extends AND_2[r32, rm32] {
      def opEn = RM()
      val opcode: OpcodeFormat = 0x23
  } 
}

object AND extends ANDLow {

  implicit object and2 extends AND_2[r16, rm16] {
      def opEn = RM()
      val opcode: OpcodeFormat = 0x23
  }
  
  implicit object and3 extends AND_2[rm8, imm8] {
      def opEn = MI()
      val opcode = 0x80 /+ 4
  }
  
  implicit object and4 extends AND_2[rm16, imm16] {
      def opEn = MI()
      val opcode = 0x81 /+ 4
  }
  
  implicit object and5 extends AND_2[rm32, imm32] {
      def opEn = MI()
      val opcode = 0x81 /+ 4
  }
  
  implicit object and6 extends AND_2[rm16, imm8] {
      def opEn = MI()
      val opcode = 0x83 /+ 4
  }
  
  implicit object and7 extends AND_2[rm32, imm8] {
      def opEn = MI()
      val opcode = 0x83 /+ 4
  }
  
  implicit object and9 extends AND_2[rm16, r16] {
      def opEn = MR()
      val opcode: OpcodeFormat = 0x21
  }
  
  implicit object and10 extends AND_2[rm32, r32] {
      def opEn = MR()
      val opcode: OpcodeFormat = 0x21
  }
  
  implicit object and11 extends AND_2[r8, rm8] {
      def opEn = RM()
      val opcode: OpcodeFormat = 0x22
  }
}