package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._
import scala.annotation.implicitNotFound
import com.scalaAsm.utils.Endian

trait SHR extends ModRM with Operands
trait SHR_2[-O1, -O2] extends SHR {
  def get(p1: O1, p2: O2): Array[Byte]
}

object SHR extends Instruction {
  implicit object shr1 extends SHR_2[r32, imm8] { def get(x: r32, y: imm8) = 0xC1.toByte +: modRM2Extended(x, y, extensionCode = 5.toByte).getBytes }
}