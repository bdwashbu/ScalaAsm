package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.TwoOperands
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.Opcodes
import com.scalaAsm.x86.OperandFormat
import com.scalaAsm.x86.{x86Instruction, OneOpcode, TwoOpcodes}

abstract class SUB extends x86Instruction("SUB")

trait SUB_2[-O1, -O2] extends SUB with TwoOperands[O1,O2] with OperandEncoding

object SUB {
  
  implicit object sub1 extends SUB_2[r32, imm8] {
      val opcode = 0x83 /+ 5
      def operands = MI(x,y)
  }
  
  implicit object sub2 extends SUB_2[r32, rm32] {
    val opcode: Opcodes = 0x2B
      def operands = RM(x,y)
      
  }
}