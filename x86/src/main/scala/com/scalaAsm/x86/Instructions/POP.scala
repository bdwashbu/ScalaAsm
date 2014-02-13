package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._
import scala.annotation.implicitNotFound
import com.scalaAsm.utils.Endian

trait POP extends ModRM with Operands
trait POP_O[-O1] extends POP {
  def get(p1: O1): Array[Byte]
}

object POP {
  implicit object pop1 extends POP_O[r32] { def get(register: r32) = Array((0x58 + register.ID).toByte) }
  implicit object pop2 extends POP_O[DS] { def get(x: DS) = Array(0x1F.toByte) }
}