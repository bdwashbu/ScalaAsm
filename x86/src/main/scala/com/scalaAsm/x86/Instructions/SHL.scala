package com.scalaAsm.x86
package Instructions

abstract class SHL extends x86Instruction("SHL")

trait SHL_2[-O1, -O2] extends SHL with TwoOperandInstruction[O1,O2] with InstructionFormat

object SHL {
  
  implicit object shl1 extends SHL_2[rm8, One] {
      def operands = M1()
      val opcode = OneOpcode(0xD0) /+ 4
  }
}
