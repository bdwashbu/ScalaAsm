package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OneOperand
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{OpcodePlusRd, ModRM, x86Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.Instruction

abstract class POP extends x86Instruction("POP")

trait POP_1[-O1] extends POP with OneOperand[O1] with OperandEncoding

object POP {
  
  implicit object pop1 extends POP_1[r32] {
      def operands = O(x)
      def opcode = OpcodePlusRd(0x58, x)
  }
  
  implicit object pop2 extends POP_1[DS] {
      def operands = new OneOperand[DS](x) {def getAddressingForm = null}
      val opcode = OneOpcode(0x1F)
  }
}