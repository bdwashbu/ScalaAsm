package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Instruction
import com.scalaAsm.x86.OneOperand
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

abstract class JNZ extends x86Instruction("JNZ")

trait JNZ_1[-O1] extends JNZ with OneOperand[O1] with OperandEncoding

object JNZ {
  
  implicit object jnz1 extends JNZ_1[imm8] {
      def operands = new OneOperand[imm8](x) {def getAddressingForm = null}
      def opcode = OneOpcode(0x75)
  }
}