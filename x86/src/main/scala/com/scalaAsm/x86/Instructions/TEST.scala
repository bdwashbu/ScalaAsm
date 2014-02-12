package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._

trait TEST extends ModRMFormat with Operands
trait TEST_1[O1] extends TEST {
  def get(p1: O1): Array[Byte]
}
trait TEST_2[-O1, -O2] extends TEST {
  def get(p1: O1, p2: O2): Array[Byte]
}

object TEST extends Instruction {
  implicit object test1 extends TEST_2[r32, rm32] { def get(x: r32, y: rm32) = 0x85.toByte +: modRM2(x, y) }
  implicit object test2 extends TEST_2[r32, imm32] { def get(x: r32, y: imm32) = 0xF7.toByte +: modRM2Extended(x, y, 0) } //Array(0xF7.toByte, 0xC1.toByte) ++ Endian.swap(y) }
}