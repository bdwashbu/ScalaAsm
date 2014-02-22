package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._

trait SHR

trait SHR_2[-O1, -O2] extends SHR {
  def get(p1: O1, p2: O2): Instruction
}

object SHR {
  
  implicit object shr1 extends SHR_2[r32, imm8] {
    def get(x: r32, y: imm8) = new Instruction {
      val operands = MI(x,y)
      val opcode = OneOpcode(0xC1) / 5
     }
  }
}