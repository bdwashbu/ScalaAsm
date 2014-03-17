package com.scalaAsm.x86
package Instructions

abstract class TEST extends x86Instruction("TEST")

trait TEST_2[-O1, -O2] extends TEST with TwoOperands[O1,O2] with OperandEncoding

object TEST {
  
  implicit object test1 extends TEST_2[r32, rm32] {
      def operands = RM(x,y)
      val opcode: Opcodes = 0x85
  }
  
  implicit object test2 extends TEST_2[r32, imm32] {
      def operands = MI(x,y)
      val opcode: Opcodes = 0xF7 /+ 0
  }
}