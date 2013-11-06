package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._
import MODRM._

  trait TEST
  trait TEST_1[O1] extends TEST {
    def get(p1: O1): Array[Byte]
  }
  trait TEST_2[-O1, -O2] extends TEST {
    def get(p1: O1, p2: O2): Array[Byte]
  }
  
   object TEST {

    implicit object test1 extends TEST_2[r32, r32] { def get(x: r32, y: r32) = 0x85.toByte +: modRM(x, y) }
    implicit object test2 extends TEST_2[r32, imm32] { def get(x: r32, y: imm32) = 0xF7.toByte +: modRM(x, y) } //Array(0xF7.toByte, 0xC1.toByte) ++ Endian.swap(y) }
  }