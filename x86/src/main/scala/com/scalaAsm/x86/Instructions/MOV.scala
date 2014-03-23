package com.scalaAsm.x86
package Instructions

abstract class MOV extends x86Instruction("MOV")

trait MOV_2[-O1, -O2] extends MOV with TwoOperands[O1,O2] with InstructionFormat

trait MOVLow {
  
 implicit object mov1 extends MOV_2[rm32, r32] {
      def operands = MR(op1,op2)
      val opcode = OneOpcode(0x89)
  }
}

object MOV extends MOVLow {
 
  implicit object mov3 extends MOV_2[r32, rm32] {
      def operands = RM(op1,op2)
      val opcode = OneOpcode(0x8B)
  }
  
  implicit object mov9 extends MOV_2[r16, rm16] {
      def operands = RM(op1,op2)
      val opcode = OneOpcode(0x8B)
  }
  
  implicit object mov7 extends MOV_2[r16, imm16] {
      def operands = OI(op1,op2) // should be OI
      def opcode = OpcodePlusRd(0xB8, op1)
  }
  
  implicit object mov8 extends MOV_2[r8, imm8] {
      def operands = OI(op1,op2)
      def opcode = OpcodePlusRd(0xB0, op1)
  }
  
   implicit object mov6 extends MOV_2[r32, imm32] {
      def operands = OI(op1,op2)
      def opcode = OpcodePlusRd(0xB8, op1)
  }
}