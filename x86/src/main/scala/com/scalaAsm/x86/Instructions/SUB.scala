package com.scalaAsm.x86
package Instructions

abstract class SUB extends x86Instruction("SUB")

trait SUB_2[-O1, -O2] extends SUB with TwoOperandInstruction[O1,O2] with InstructionFormat

object SUB {
  
  implicit object sub1 extends SUB_2[rm32, imm8] {
      val opcode = 0x83 /+ 5
      def operands = MI()
  }
  
  implicit object sub2 extends SUB_2[r32, rm32] {
    val opcode: OpcodeFormat = 0x2B
      def operands = RM()
      
  }
}