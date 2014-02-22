package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, x86Instruction, Opcodes, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._

class SBB extends x86Instruction("SBB")

trait SBB_2[-O1, -O2] extends SBB {
  def get(x: O1, y: O2): Instruction
}

object SBB {
  
  implicit object sbb1 extends SBB_2[r32, rm32] {
    def get(x: r32, y: rm32) = new Instruction {
      val operands = RM(x,y)
      val opcode: Opcodes = 0x1B
     }
  }
}