package com.scalaAsm.x86
package Instructions

abstract class RET extends x86Instruction("RET") with InstructionFormat

object RET {

  implicit object NearReturn extends RET {
      def operands = NA
      val opcode: OpcodeFormat = 0xC3
  }
}