package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OneOperand
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.Instruction

abstract class NOT extends x86Instruction("NOT")

trait NOT_1[-O1] extends NOT with OneOperand[O1] with OperandEncoding

object NOT {
  
  implicit object not1 extends NOT_1[rm32] {
      def operands = M(x)
      val opcode = 0xF7 /+ 2
  }
}