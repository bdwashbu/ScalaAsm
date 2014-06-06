package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.Displacement

trait InstructionFormat

case class Format1(opcode: OpcodeFormat) extends InstructionFormat {
  val size = 1
  val getBytes = opcode.get(null)
}

case class Format2(opcode: OpcodeFormat, imm: Immediate8) extends InstructionFormat {
  val size = 2
  val getBytes = opcode.get(null) ++ imm.getBytes
}

case class Format3(opcode: OpcodeFormat, disp: Displacement) extends InstructionFormat {
  val size = 1 + disp.size
  val getBytes = opcode.get(null) ++ disp.getBytes
}

case class Format9(opcode: OpcodeFormat) extends InstructionFormat {
  val size = 2
  val getBytes = opcode.get(null)
}