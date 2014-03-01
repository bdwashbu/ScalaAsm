package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.TwoOperands
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.Opcodes
import com.scalaAsm.x86.OperandFormat
import com.scalaAsm.x86.{x86Instruction, OneOpcode, TwoOpcodes}

abstract class ADD extends x86Instruction("ADD")

trait ADD_2[-O1, -O2] extends ADD with TwoOperands[O1,O2] with OperandEncoding

object ADD {
  
  implicit object add1 extends ADD_2[r32, imm8] {
      val opcode = 0x83 /+ 0
      def operands = MI(x,y)
  }
  
  implicit object add2 extends ADD_2[r16, imm8] {
      val opcode = 0x83 /+ 0
      def operands = MI(x,y)
  }
  
  implicit object add3 extends ADD_2[r32, rm32] {
      def operands = RM(x,y)
      val opcode: Opcodes = 0x03
  }
}