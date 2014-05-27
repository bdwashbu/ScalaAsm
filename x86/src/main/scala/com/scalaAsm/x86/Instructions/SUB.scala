package com.scalaAsm.x86
package Instructions

abstract class SUB extends x86Instruction("SUB")

trait SUB_2[-O1 <: Operand, -O2 <: Operand] extends SUB with TwoOperandInstruction[O1,O2]

object SUB {
  
  implicit object sub1 extends SUB_2[rm32, imm8] {
      val opcode = 0x83 /+ 5
      def opEn = MI
  }
  
  implicit object sub3 extends SUB_2[rm64, imm8] {
      val opcode = 0x83 /+ 5
      def opEn = MI
  }
  
  implicit object sub2 extends SUB_2[r32, rm32] {
    val opcode: OpcodeFormat = 0x2B
      def opEn = RM
      
  }
}