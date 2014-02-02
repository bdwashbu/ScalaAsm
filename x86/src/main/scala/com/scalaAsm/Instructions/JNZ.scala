package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._

trait JNZ extends ModRMFormat with Operands
trait JNZ_1[O1] extends JNZ {
  def get(p1: O1): Array[Byte]
}

object JNZ extends Instruction {
  implicit object jnz1 extends JNZ_1[imm8] { def get(x: imm8) = Array(0x75.toByte, x.value) }
}