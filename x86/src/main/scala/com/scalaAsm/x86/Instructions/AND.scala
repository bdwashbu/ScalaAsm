package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.TwoOperands
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{x86Instruction, Opcodes, OneOpcode, TwoOpcodes}

abstract class AND extends x86Instruction("AND")

trait AND_2[-O1, -O2] extends AND with TwoOperands[O1,O2] with OperandEncoding

object AND {
  
  implicit object and1 extends AND_2[r32, rm32] {
      def operands = RM(x,y)
      def opcode: Opcodes = 0x23
  }
}