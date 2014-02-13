package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._

trait INT extends ModRM with Operands
trait INT_1[O1] extends INT {
  def get(p1: O1): Array[Byte]
}

object INT extends Instruction {
  implicit object int1 extends INT_1[imm8] { def get(x: imm8) = Array(0xCD.toByte, x.value) }
}