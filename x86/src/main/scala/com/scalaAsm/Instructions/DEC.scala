package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._

trait DEC extends ModRMFormat with Operands
trait DEC_O[-O1] extends DEC {
  def get(p1: O1): Array[Byte]
}

object DEC {
  implicit object dec1 extends DEC_O[r32] { def get(x: r32) = Array((0x48 + x.ID).toByte) }
  implicit object dec2 extends DEC_O[r16] { def get(x: r16) = Array((0x48 + x.ID).toByte) }
}