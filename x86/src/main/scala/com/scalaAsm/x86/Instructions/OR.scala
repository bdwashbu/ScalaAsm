package com.scalaAsm.x86
package Instructions

abstract class OR extends x86Instruction("OR")

trait OR_2[-O1,-O2] extends OR with TwoOperandInstruction[O1,O2]

object OR {
  
  implicit object or1 extends OR_2[rm8, imm8] {
      def opEn = MI()
      val opcode = 0x80 /+ 1
  }
}