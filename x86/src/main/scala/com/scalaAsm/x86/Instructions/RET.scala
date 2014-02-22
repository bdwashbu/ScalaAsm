package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.{Instruction, OneOpcode}

trait RET

object RET {
  
  object NearReturn extends Instruction {
      val operands = NA
      val opcode = OneOpcode(0xC3)
  }
}