package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.{x86Instruction, Opcodes}

abstract class LEAVE extends x86Instruction("LEAVE") {
  def get: Instruction
}

object LEAVE {
  
  implicit object lea1 extends LEAVE {
    def get = new Instruction {
      val operands = NA
      val opcode: Opcodes = 0xC9
     }
  }
}