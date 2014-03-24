package com.scalaAsm.x86
package Instructions

abstract class SBB extends x86Instruction("SBB")

trait SBB_2[-O1, -O2] extends SBB with TwoOperands[O1,O2] with InstructionFormat

object SBB {
  
  implicit object sbb1 extends SBB_2[r32, rm32] {
      def operands = RM(op1,op2)
      val opcode: OpcodeFormat = 0x1B
  }
}