package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import MODRM._

trait INT
trait INT_1[O1] extends INT {
  def get(p1: O1): Array[Byte]
}

object INT {
  implicit object int1 extends INT_1[imm8] { def get(x: imm8) = Array(0xCD.toByte, x.value) }
}