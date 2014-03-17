package com.scalaAsm.x86
package Instructions

abstract class SBB extends x86Instruction("SBB")

trait SBB_2[-O1, -O2] extends SBB with TwoOperands[O1,O2] with OperandEncoding

object SBB {
  
  implicit object sbb1 extends SBB_2[r32, rm32] {
      def operands = RM(x,y)
      val opcode: Opcodes = 0x1B
  }
}