package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._

trait ADD extends ModRM with Operands
trait ADD_2[-O1, -O2] extends ADD {
  def get(op1: O1, op2: O2): Array[Byte]
}

object ADD extends Instruction {
  implicit object add1 extends ADD_2[r32, imm8] { def get(x: r32, y: imm8) = 0x83.toByte +: modRM(x, y, reg = 0.toByte) }
}