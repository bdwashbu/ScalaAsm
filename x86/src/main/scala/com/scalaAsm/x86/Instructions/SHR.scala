package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.TwoOperands
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.Instruction

abstract class SHR extends x86Instruction("SHR")

trait SHR_2[-O1, -O2] extends SHR with TwoOperands[O1,O2] with OperandEncoding

object SHR {
  
  implicit object shr1 extends SHR_2[r32, imm8] {
      def operands = MI(x,y)
      def opcode = 0xC1 /+ 5
  }
}