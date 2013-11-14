package com.scalaAsm.x86

import com.scalaAsm.utils.Endian
import x86Registers._
import Addressing._

private[x86] trait ModRM {
  self: Operands =>

  def modRM[O1, O2](p1: O1, p2: O2, reg: Byte = 0)(implicit ev: MODRM_2[O1, O2]) = {
    ev.reg = reg
    ev.get(p1, p2)
  }

  def modRM[O1](p1: O1, reg: Byte = 0)(implicit ev: MODRM_1[O1]) = {
    ev.reg = reg
    ev.get(p1)
  }
  
  protected[this] trait MODRM {
    var reg: Byte = 0
  }
  
  protected[this] trait MODRM_2[-O1, -O2] extends MODRM {
    def get(p1: O1, p2: O2): Array[Byte]
  }

  protected[this] trait MODRM_1[-O1] extends MODRM {
    def get(p1: O1): Array[Byte]
  }

  implicit object mod1 extends MODRM_2[r32, r32] { 
    def get(x: r32, y: r32) = Array((0xC0 + 8 * x.ID + y.ID).toByte)
  }
  
  implicit object mod11 extends MODRM_2[r16, r16] {
    def get(x: r16, y: r16) = Array((0xC0 + 8 * x.ID + y.ID).toByte)
  }
  
  implicit object mod2 extends MODRM_2[r32, *[r32]] { 
    def get(x: r32, y: *[r32]) = Array((8 * x.ID + y.x.ID).toByte)
  }
  
  implicit object mod3 extends MODRM_2[*[r32 + imm8], r32] {
    def get(x: *[r32 + imm8], y: r32) = mod4.get(y, x)
  }
  
  implicit object mod6 extends MODRM_1[*[r32 + imm8]] {
    def get(x: *[r32 + imm8]) = Array((0x40 + reg * 8 + x.x.x.ID).toByte, x.x.offset.value)
  }
  
  implicit object mod8 extends MODRM_1[Register[_]] {
    def get(x: Register[_]) = Array((0xC0 + reg * 8 + x.ID).toByte)
  }
  
  implicit object mod7 extends MODRM_2[r32, imm8] {
    def get(x: r32, y: imm8) = Array((0xC0 + reg * 8 + x.ID).toByte, y.value)
  }
  
  implicit object mod10 extends MODRM_2[r32, imm32] {
    def get(x: r32, y: imm32) = Array((0xC0 + reg * 8 + x.ID).toByte) ++ Endian.swap(y.value)
  }
  
  implicit object mod4 extends MODRM_2[r32, *[r32 + imm8]] {
    def get(x: r32, y: *[r32 + imm8]) = {
      if (y.x.x.ID == 4) // [--][--] SIB
        Array((0x40 + 8 * x.ID + y.x.x.ID).toByte, 0x24.toByte, y.x.offset.value)
      else
        Array((0x40 + 8 * x.ID + y.x.x.ID).toByte, y.x.offset.value)
    }
  }
  
  implicit object mod5 extends MODRM_2[r32, *[r32 + imm32]] {
    def get(x: r32, y: *[r32 + imm32]) = { Array(0x8D.toByte, 0x8F.toByte) ++ Endian.swap(y.x.offset.value) }
  }
}