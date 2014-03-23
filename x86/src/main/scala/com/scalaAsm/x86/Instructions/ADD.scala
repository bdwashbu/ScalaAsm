package com.scalaAsm.x86
package Instructions

abstract class ADD extends x86Instruction("ADD")

trait ADD_2[-O1, -O2] extends ADD with TwoOperands[O1,O2] with InstructionFormat

object ADD {
  
  implicit object add1 extends ADD_2[rm32, imm8] {
      val opcode = 0x83 /+ 0
      def operands = MI(op1,op2)
  }
  
  implicit object add2 extends ADD_2[rm16, imm8] {
      val opcode = 0x83 /+ 0
      def operands = MI(op1,op2)
  }
  
  implicit object add3 extends ADD_2[r32, rm32] {
      def operands = RM(op1,op2)
      val opcode: Opcodes = 0x03
  }
}