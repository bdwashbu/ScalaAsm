package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._
import scala.annotation.implicitNotFound
import com.scalaAsm.utils.Endian

trait SBB extends ModRM with Operands
trait SBB_2[-O1, -O2] extends SBB {
  def get(x: O1, y: O2): Array[Byte]
}

object SBB {
  implicit object sbb1 extends SBB_2[r32, rm32] { def get(x: r32, y: rm32) = 0x1B.toByte +: modRM2(x, y) }
}