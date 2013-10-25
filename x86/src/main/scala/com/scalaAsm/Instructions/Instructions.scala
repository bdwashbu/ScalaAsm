package com.scalaAsm.x86

import com.scalaAsm.utils.Endian

trait Instructions extends x86Registers with Addressing {

  trait ADD
  trait ADD_2[-O1, -O2] extends ADD {
    def get(op1: O1, op2: O2): Array[Byte]
  }

  trait POP
  trait POP_O[-O1] extends POP {
    def get(p1: O1): Array[Byte]
  }

  trait PUSH

  trait PUSH_M[-O1] extends PUSH {
    def get(op1: O1): Array[Byte]
  }

  trait PUSH_I[O1] extends PUSH {
    var x: O1 = _
    def get: Array[Byte]
  }

  trait AND
  trait AND_RM[-O1, -O2] extends AND {
    def get(p1: O1, p2: O2): Array[Byte]
  }

  trait NOT
  trait NOT_M[-O1] extends NOT {
    def get(p: O1): Array[Byte]
  }

  trait LEA
  trait LEA_3[O1, O2, O3] extends LEA {
    def get: Array[Byte]
  }
  trait LEA_2[-O1, -O2] extends LEA {
    //var x: (O1, O2) = _
    def get(p1: O1, p2: O2): Array[Byte]
  }

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

  trait SHR
  trait SHR_2[-O1, -O2] extends SHR {
    def get(p1: O1, p2: O2): Array[Byte]
  }

  trait JNZ
  trait JNZ_1[O1] extends JNZ {
    def get(p1: O1): Array[Byte]
  }

  trait JZ
  trait JZ_1[O1] extends JZ {
    def get(p1: O1): Array[Byte]
  }
  
  trait INT
  trait INT_1[O1] extends INT {
    def get(p1: O1): Array[Byte]
  }

  trait SHL
  trait SHL_1[-O1] extends SHL {
    def get(p: O1): Array[Byte]
  }

  trait SBB
  trait SBB_2[-O1, -O2] extends SBB {
    def get(x: O1, y: O2): Array[Byte]
  }

  trait RETN
  trait RETN_1[O1] extends RETN {
    def get(p1: O1): Array[Byte]
  }

  trait TEST
  trait TEST_1[O1] extends TEST {
    def get(p1: O1): Array[Byte]
  }
  trait TEST_2[-O1, -O2] extends TEST {
    def get(p1: O1, p2: O2): Array[Byte]
  }

  type +[A <: Register[_], B <: Immediate[_, _]] = RegisterOffset[A, B]

  trait MODRM[-O1, -O2] {
    var reg: Byte = 0
    def get(p1: O1, p2: O2): Array[Byte]
  }

  trait MODRM_1[-O1] {
    var reg: Byte = 0
    def get(p1: O1): Array[Byte]
  }

  object MODRM
  {
	  implicit object mod1 extends MODRM[r32, r32] { def get(x: r32, y: r32) = { Array((0xC0 + 8 * x.ID + y.ID).toByte) } }
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
  
  def modRM[O1, O2](p1: O1, p2: O2, reg: Byte = 0)(implicit ev: MODRM[O1, O2]) = {
    ev.reg = reg
    ev.get(p1, p2)
  }

  def modRM[O1](p1: O1, reg: Byte = 0)(implicit ev: MODRM_1[O1]) = {
    ev.reg = reg
    ev.get(p1)
  }

  object TEST {
    implicit object test1 extends TEST_2[r32, r32] { def get(x: r32, y: r32) = 0x85.toByte +: modRM(x, y) }
    implicit object test2 extends TEST_2[r32, imm32] { def get(x: r32, y: imm32) = 0xF7.toByte +: modRM(x, y) } //Array(0xF7.toByte, 0xC1.toByte) ++ Endian.swap(y) }
  }

  object RETN {
    implicit object retn1 extends RETN_1[imm16] { def get(x: imm16) = Array(0xC2.toByte) ++ Endian.swap(x.value) }
  }

  object SBB {
    implicit object sbb1 extends SBB_2[r32, r32] { def get(x: r32, y: r32) = 0x1B.toByte +: modRM(x, y) }
  }

  object SHL {
    import MODRM._
    implicit object shl1 extends SHL_1[r8] { def get(x: r8) = 0xD0.toByte +: modRM(x, reg = 4.toByte) }
  }

  object SHR {
    implicit object shr1 extends SHR_2[r32, imm8] { def get(x: r32, y: imm8) = 0xC1.toByte +: modRM(x, y, reg = 5.toByte) }
  }

  object JNZ {
    implicit object jnz1 extends JNZ_1[imm8] { def get(x: imm8) = Array(0x75.toByte, x.value) }
  }

  object JZ {
    implicit object jz1 extends JZ_1[imm8] { def get(x: imm8) = Array(0x74.toByte, x.value) }
  }

  object ADD {
    implicit object add1 extends ADD_2[r32, imm8] { def get(x: r32, y: imm8) = 0x83.toByte +: modRM(x, y, reg = 0.toByte) }
  }
  
  object INT {
    implicit object int1 extends INT_1[imm8] { def get(x: imm8) = Array(0xCD.toByte, x.value) }
  }

  object PUSH {
    import MODRM._
    implicit object push1 extends PUSH_M[r32] { def get(x: r32) = Array((0x50 + x.ID).toByte) }
    implicit object push4 extends PUSH_M[imm8] { def get(x: imm8) = Array(0x6A.toByte, x.value) }
    implicit object push5 extends PUSH_M[imm16] { def get(x: imm16) = Array(0x68.toByte) ++ Endian.swap(x.value) }
    implicit object push6 extends PUSH_M[*[r32 + imm8]] { def get(x: *[r32 + imm8]) = 0xFF.toByte +: modRM(x, reg = 6.toByte) }
    implicit object push7 extends PUSH_M[CS] { def get(x: CS) = Array(0x0E.toByte) }
  }

  object POP {
    implicit object pop1 extends POP_O[r32] { def get(register: r32) = Array((0x58 + register.ID).toByte) }
    implicit object pop2 extends POP_O[DS] { def get(x: DS) = Array(0x1F.toByte) }
  }

  object AND {
    implicit object and1 extends AND_RM[r32, r32] { def get(x: r32, y: r32) = 0x23.toByte +: modRM(x, y) }
  }

  object NOT {
    import MODRM._
    implicit object not1 extends NOT_M[r32] { def get(x: r32) = 0xF7.toByte +: modRM(x, reg = 2) }
  }

  object LEA {
    implicit object lea1 extends LEA_2[r32, *[r32 + imm8]] { def get(x: r32, y: *[r32 + imm8]) = 0x8D.toByte +: modRM(x, y) }
    implicit object lea3 extends LEA_2[r32, *[r32 + imm32]] { def get(x: r32, y: *[r32 + imm32]) = Array(0x8D.toByte, 0x8F.toByte) ++ Endian.swap(y.x.offset.value) }
  }

  object MOV {
    implicit object mov1 extends MOV_R[*[r32 + imm8], r32] { def get(x: *[r32 + imm8], y: r32) = 0x89.toByte +: modRM(x, y) }

    implicit object mov3 extends MOV_R[r32, *[r32 + imm8]] { def get(x: r32, y: *[r32 + imm8]) = 0x8B.toByte +: modRM(x, y) }

    implicit object mov4 extends MOV_R[r32, r32] { def get(x: r32, y: r32) = 0x8B.toByte +: modRM(x, y) }

    implicit object mov5 extends MOV_R[r32, *[r32]] { def get(x: r32, y: *[r32]) = 0x8B.toByte +: modRM(x, y) }
    
    implicit object mov7 extends MOV_R[r16, imm16] { def get(x: r16, y: imm16) = Array[Byte]((0xB8 + x.ID).toByte, (y.value & 0x00FF).toByte, ((y.value & 0xFF00) >> 8).toByte) }
    implicit object mov8 extends MOV_R[r8, imm8] { def get(x: r8, y: imm8) = Array[Byte]((0xB0 + x.ID).toByte, y.value) }

    implicit object mov6 extends MOV_R2[imm32] { def get(x: imm32) = 0xBD.toByte +: Endian.swap(x.value) }
  }
}