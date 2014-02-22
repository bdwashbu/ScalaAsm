package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{x86Instruction, Opcodes, OneOpcode, TwoOpcodes}

class AND extends x86Instruction("AND")

trait AND_2[-O1, -O2] extends AND {
  def get(p1: O1, p2: O2): Instruction
}

object AND {
  
  implicit object and1 extends AND_2[r32, rm32] {
    def get(x: r32, y: rm32) = new Instruction {
      val operands = RM(x,y)
      val opcode: Opcodes = 0x23
     }
  }
}