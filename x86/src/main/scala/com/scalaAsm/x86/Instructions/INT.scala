package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Instruction
import com.scalaAsm.x86.OneOperand
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

abstract class INT extends x86Instruction("INT")

trait INT_1[-O1] extends INT with OneOperand[O1] with OperandEncoding

object INT {

  implicit object int1 extends INT_1[imm8] {
    def operands = I[imm8](x)
    val opcode = OneOpcode(0xCD)
  }
}