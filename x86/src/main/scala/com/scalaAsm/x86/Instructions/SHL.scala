package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.TwoOperands
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand, ByteOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.Instruction

abstract class SHL extends x86Instruction("SHL")

trait SHL_2[-O1, -O2] extends SHL with TwoOperands[O1,O2] with OperandEncoding

object SHL {
  
  implicit object shl1 extends SHL_2[rm8, One] {
      def operands = M1(x)
      def opcode = OneOpcode(0xD0) /+ 4
  }
}
