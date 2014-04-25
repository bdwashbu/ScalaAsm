package com.scalaAsm.x86
package Instructions

abstract class SHR extends x86Instruction("SHR")

trait SHR_2[-O1, -O2] extends SHR with TwoOperandInstruction[O1,O2]

object SHR {
  
  implicit object shr1 extends SHR_2[rm32, imm8] {
      def operands = MI()
      val opcode = 0xC1 /+ 5
  }
}