package com.scalaAsm.x86
package Instructions

abstract class MUL extends x86Instruction("MUL")

trait MUL_1[-O1 <: Operand] extends MUL with OneOperandInstruction[O1]

object MUL {
  
  implicit object mul1 extends MUL_1[rm32] {
      val opcode = 0xF7 /+ 4
      def opEn = M()
  }
}