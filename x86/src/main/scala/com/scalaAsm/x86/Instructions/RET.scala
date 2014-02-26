package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.{x86Instruction, Opcodes, OneOpcode}
import com.scalaAsm.x86.Instruction
import com.scalaAsm.x86.OperandEncoding

abstract class RET extends x86Instruction("RET") with OperandEncoding

object RET {

  implicit object NearReturn extends RET {
      def operands = NA
      def opcode: Opcodes = 0xC3
  }
}