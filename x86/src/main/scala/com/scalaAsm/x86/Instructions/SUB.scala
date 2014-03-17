package com.scalaAsm.x86
package Instructions

abstract class SUB extends x86Instruction("SUB")

trait SUB_2[-O1, -O2] extends SUB with TwoOperands[O1,O2] with OperandEncoding

object SUB {
  
  implicit object sub1 extends SUB_2[r32, imm8] {
      val opcode = 0x83 /+ 5
      def operands = MI(x,y)
  }
  
  implicit object sub2 extends SUB_2[r32, rm32] {
    val opcode: Opcodes = 0x2B
      def operands = RM(x,y)
      
  }
}