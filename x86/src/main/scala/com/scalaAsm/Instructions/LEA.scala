package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._
import com.scalaAsm.utils.Endian

trait LEA extends ModRMFormat with Operands
trait LEA_3[O1, O2, O3] extends LEA {
  def get: Array[Byte]
}
trait LEA_2[-O1, -O2] extends LEA {
  def get(p1: O1, p2: O2): Array[Byte]
}

object LEA extends Instruction {
  implicit object lea1 extends LEA_2[r32, *[r32 + imm8]] { def get(x: r32, y: *[r32 + imm8]) = 0x8D.toByte +: modRM2(x, y) }
  implicit object lea3 extends LEA_2[r32, *[r32 + imm32]] { def get(x: r32, y: *[r32 + imm32]) = Array(0x8D.toByte, 0x8F.toByte) ++ Endian.swap(y.x.offset.value) }
}