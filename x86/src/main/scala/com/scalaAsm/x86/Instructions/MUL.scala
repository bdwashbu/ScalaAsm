package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OneOperand
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.Opcodes
import com.scalaAsm.x86.OperandFormat
import com.scalaAsm.x86.{x86Instruction, OneOpcode, TwoOpcodes}

abstract class MUL extends x86Instruction("MUL")

trait MUL_1[-O1] extends MUL with OneOperand[O1] with OperandEncoding

object MUL {
  
  implicit object mul1 extends MUL_1[rm32] {
      val opcode = 0xF7 /+ 4
      def operands = M(x)
  }
}