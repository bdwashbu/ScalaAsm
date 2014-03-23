package com.scalaAsm.x86
package Instructions

abstract class MUL extends x86Instruction("MUL")

trait MUL_1[-O1] extends MUL with OneOperand[O1] with InstructionFormat

object MUL {
  
  implicit object mul1 extends MUL_1[rm32] {
      val opcode = 0xF7 /+ 4
      def operands = M(op1)
  }
}