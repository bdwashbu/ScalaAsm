package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import MODRM._

trait JZ
trait JZ_1[O1] extends JZ {
  def get(p1: O1): Array[Byte]
}

object JZ {
  implicit object jz1 extends JZ_1[imm8] { def get(x: imm8) = Array(0x74.toByte, x.value) }
}