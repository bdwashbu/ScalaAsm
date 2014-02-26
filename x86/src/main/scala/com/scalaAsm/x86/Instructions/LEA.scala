package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Instruction
import com.scalaAsm.x86.TwoOperands
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

abstract class LEA extends x86Instruction("LEA")

trait LEA_2[-O1, -O2] extends LEA with TwoOperands[O1,O2] with OperandEncoding

object LEA {

  implicit object lea1 extends LEA_2[r32, rm32] {
      def operands = RM(x,y)
      def opcode = 0x8D /+ 0
  }
}