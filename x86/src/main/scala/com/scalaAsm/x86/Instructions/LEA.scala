package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait LEA

trait LEA_2[-O1, -O2] extends LEA {
  def get(p1: O1, p2: O2): Instruction
}

object LEA {

  implicit object lea1 extends LEA_2[r32, rm32] {
    def get(x: r32, y: rm32) = new Instruction {
      val operands = RM(x,y)
      val opcode = OneOpcode(0x8D) / 0
     }
  }
}