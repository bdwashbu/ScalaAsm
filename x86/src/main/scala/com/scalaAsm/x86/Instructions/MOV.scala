package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._
import com.scalaAsm.utils.Endian

trait MOV
trait MOV_RM[O1, O2, O3] extends MOV {
  def get(x: O3): Array[Byte]
}

trait MOV_R[-O1, -O2] extends MOV {
  def get(op1: O1, op2: O2): Array[Byte]
}

trait MOV_R2[O1] extends MOV {
  def get(x: O1): Array[Byte]
}

trait MOVLow extends ModRM with Operands {
   implicit object mov1 extends MOV_R[rm32, r32] { def get(x: rm32, y: r32) = 0x89.toByte +: modRM2(x, y) }
   
}

object MOV extends MOVLow {
 
implicit object mov3 extends MOV_R[r32, rm32] { def get(x: r32, y: rm32) = 0x8B.toByte +: modRM2(x, y) }  
  implicit object mov9 extends MOV_R[r16, rm16] { def get(x: r16, y: rm16) = 0x8B.toByte +: modRM2(x, y) }

  implicit object mov7 extends MOV_R[r16, imm16] { def get(x: r16, y: imm16) = Array[Byte]((0xB8 + x.ID).toByte, (y.value & 0x00FF).toByte, ((y.value & 0xFF00) >> 8).toByte) }
  implicit object mov8 extends MOV_R[r8, imm8] { def get(x: r8, y: imm8) = Array[Byte]((0xB0 + x.ID).toByte, y.value) }

  implicit object mov6 extends MOV_R[r32, imm32] { def get(x: r32, y: imm32) = (0xB8 + x.ID).toByte +: Endian.swap(y.value) }
}