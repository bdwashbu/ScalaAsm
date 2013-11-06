package com.scalaAsm.x86

import com.scalaAsm.utils.Endian
      import x86Registers._
      import Addressing._


  trait MODRM[-O1, -O2] {
    var reg: Byte = 0
    def get(p1: O1, p2: O2): Array[Byte]
  }

  trait MODRM_1[-O1] {
    var reg: Byte = 0
    def get(p1: O1): Array[Byte]
  }

 object MODRM extends Operands
  {
	  implicit object mod1 extends MODRM[r32, r32] { def get(x: r32, y: r32) = { Array((0xC0 + 8 * x.ID + y.ID).toByte) } }
	  implicit object mod11 extends MODRM[r16, r16] { def get(x: r16, y: r16) = { Array((0xC0 + 8 * x.ID + y.ID).toByte) } }
	  implicit object mod2 extends MODRM[r32, *[r32]] { def get(x: r32, y: *[r32]) = { Array((8 * x.ID + y.x.ID).toByte) } }
	  implicit object mod3 extends MODRM[*[r32 + imm8], r32] { def get(x: *[r32 + imm8], y: r32) = mov4.get(y, x) }
	  implicit object mod6 extends MODRM_1[*[r32 + imm8]] { def get(x: *[r32 + imm8]) = Array((0x40 + reg * 8 + x.x.x.ID).toByte, x.x.offset.value) }
	  implicit object mod8 extends MODRM_1[Register[_]] { def get(x: Register[_]) = Array((0xC0 + reg * 8 + x.ID).toByte) }
	  implicit object mod7 extends MODRM[r32, imm8] { def get(x: r32, y: imm8) = Array((0xC0 + reg * 8 + x.ID).toByte, y.value) }
	  implicit object mod10 extends MODRM[r32, imm32] { def get(x: r32, y: imm32) = Array((0xC0 + reg * 8 + x.ID).toByte) ++ Endian.swap(y.value) }
	  implicit object mov4 extends MODRM[r32, *[r32 + imm8]] {
	    def get(x: r32, y: *[r32 + imm8]) = {
	      if (y.x.x.ID == 4) // [--][--] SIB
	        Array((0x40 + 8 * x.ID + y.x.x.ID).toByte, 0x24.toByte, y.x.offset.value)
	      else
	        Array((0x40 + 8 * x.ID + y.x.x.ID).toByte, y.x.offset.value)
	    }
	  }
	  implicit object mov5 extends MODRM[r32, *[r32 + imm32]] { def get(x: r32, y: *[r32 + imm32]) = { Array(0x8D.toByte, 0x8F.toByte) ++ Endian.swap(y.x.offset.value) } }
  }
  
