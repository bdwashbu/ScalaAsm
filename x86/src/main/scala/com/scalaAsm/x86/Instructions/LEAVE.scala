package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.{x86Instruction, Opcodes}
import com.scalaAsm.x86.Instruction
import com.scalaAsm.x86.OperandEncoding

abstract class LEAVE extends x86Instruction("LEAVE") with OperandEncoding

object LEAVE {
  
  implicit object lea1 extends LEAVE {
      def operands = NA
      def opcode: Opcodes = 0xC9
  }
}