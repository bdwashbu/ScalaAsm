package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._
import scala.annotation.implicitNotFound
import com.scalaAsm.utils.Endian

trait NOT extends ModRM with Operands
trait NOT_M[-O1] extends NOT {
  def get(p: O1): Array[Byte]
}

object NOT {
  implicit object not1 extends NOT_M[r32] { def get(x: r32) = 0xF7.toByte +: modRM(x, reg = 2) }
}