package com.scalaAsm.x86
package Instructions

abstract class SHL extends x86Instruction("SHL")

trait SHL_2[-O1 <: Operand, -O2 <: Operand] extends SHL with TwoOperandInstruction[O1,O2]

object SHL {
  
  implicit object shl1 extends SHL_2[rm8, One] {
      def opEn = M1
      val opcode = OneOpcode(0xD0) /+ 4
  }
}
