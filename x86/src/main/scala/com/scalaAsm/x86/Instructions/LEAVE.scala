package com.scalaAsm.x86
package Instructions

abstract class LEAVE extends x86Instruction("LEAVE") with OperandEncoding

object LEAVE {
  
  implicit object lea1 extends LEAVE {
      def operands = NA
      val opcode: Opcodes = 0xC9
  }
}