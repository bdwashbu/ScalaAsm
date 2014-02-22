package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.{x86Instruction, Opcodes, OneOpcode}

abstract class RET extends x86Instruction("RET") {
  def get: Instruction
}

object RET {

  implicit object NearReturn extends RET {
    def get = new Instruction {
      val operands = NA
      val opcode: Opcodes = 0xC3
     }
  }
}