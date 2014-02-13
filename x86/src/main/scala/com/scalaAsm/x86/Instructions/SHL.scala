package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._
import scala.annotation.implicitNotFound
import com.scalaAsm.utils.Endian

trait SHL extends ModRM with Operands
trait SHL_1[-O1] extends SHL {
  def get(p: O1): Array[Byte]
}

object SHL {
  implicit object shl1 extends SHL_1[rm8] { def get(x: rm8) = 0xD0.toByte +: modRMExtended(x, extensionCode = 4.toByte) }
}