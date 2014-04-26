package com.scalaAsm.x86
package Instructions

abstract class SBB extends x86Instruction("SBB")

trait SBB_2[-O1, -O2] extends SBB with TwoOperandInstruction[O1,O2]

object SBB {
  
  implicit object sbb1 extends SBB_2[r32, rm32] {
      def opEn = RM()
      val opcode: OpcodeFormat = 0x1B
  }
}