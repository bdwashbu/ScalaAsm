package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.TwoOperands
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{ModRM, x86Instruction, Opcodes, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.Instruction

abstract class SBB extends x86Instruction("SBB")

trait SBB_2[-O1, -O2] extends SBB with TwoOperands[O1,O2] with OperandEncoding

object SBB {
  
  implicit object sbb1 extends SBB_2[r32, rm32] {
      def operands = RM(x,y)
      val opcode: Opcodes = 0x1B
  }
}