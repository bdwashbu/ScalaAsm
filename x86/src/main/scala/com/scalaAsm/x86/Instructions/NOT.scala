package com.scalaAsm.x86
package Instructions

abstract class NOT extends x86Instruction("NOT")

trait NOT_1[-O1 <: Operand] extends NOT with OneOperandInstruction[O1]

object NOT {
  
  implicit object not1 extends NOT_1[rm32] {
      def opEn = M()
      val opcode = 0xF7 /+ 2
  }
}