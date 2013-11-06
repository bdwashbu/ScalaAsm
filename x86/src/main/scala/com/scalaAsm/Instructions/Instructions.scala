package com.scalaAsm.x86

import com.scalaAsm.utils.Endian
import scala.annotation.implicitNotFound
import MODRM._
import x86Registers._
import Addressing._
import MODRM._

//trait Instructions extends Operands {
  


  trait ADD
  trait ADD_2[-O1, -O2] extends ADD {
    def get(op1: O1, op2: O2): Array[Byte]
  }

  trait POP
  trait POP_O[-O1] extends POP {
    def get(p1: O1): Array[Byte]
  }

  trait PUSH

  @implicitNotFound(msg = "Cannot find PUSH implementation for ${O1}")
  trait PUSH_M[-O1] extends PUSH {
    def get(op1: O1): Array[Byte]
  }

  trait AND
  trait AND_RM[-O1, -O2] extends AND {
    def get(p1: O1, p2: O2): Array[Byte]
  }
  
  trait DEC
  trait DEC_O[-O1] extends DEC {
    def get(p1: O1): Array[Byte]
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

  trait RDRAND
  trait RDRAND_M[-O1] extends RDRAND {
    def get(p1: O1): Array[Byte]
  }
  
  trait RETN
  trait RETN_1[O1] extends RETN {
    def get(p1: O1): Array[Byte]
  }



  



 

  object RETN {
    implicit object retn1 extends RETN_1[imm16] { def get(x: imm16) = Array(0xC2.toByte) ++ Endian.swap(x.value) }
  }

  object DEC {
    implicit object dec1 extends DEC_O[r32] { def get(x: r32) = Array((0x48 + x.ID).toByte) }
    implicit object dec2 extends DEC_O[r16] { def get(x: r16) = Array((0x48 + x.ID).toByte) }
  }
  
  object SBB {
    implicit object sbb1 extends SBB_2[r32, r32] { def get(x: r32, y: r32) = 0x1B.toByte +: modRM(x, y) }
  }

  object SHL {
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
    implicit object push1 extends PUSH_M[r32] { def get(x: r32) = Array((0x50 + x.ID).toByte) }
    implicit object push8 extends PUSH_M[r16] { def get(x: r16) = Array((0x50 + x.ID).toByte) }
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
    implicit object not1 extends NOT_M[r32] { def get(x: r32) = 0xF7.toByte +: modRM(x, reg = 2) }
  }

  object LEA {
    implicit object lea1 extends LEA_2[r32, *[r32 + imm8]] { def get(x: r32, y: *[r32 + imm8]) = 0x8D.toByte +: modRM(x, y) }
    implicit object lea3 extends LEA_2[r32, *[r32 + imm32]] { def get(x: r32, y: *[r32 + imm32]) = Array(0x8D.toByte, 0x8F.toByte) ++ Endian.swap(y.x.offset.value) }
  }

  object RDRAND {
    implicit object rdrand1 extends RDRAND_M[r32] { def get(x: r32) = Array[Byte](0x0F.toByte, 0xC7.toByte) ++ modRM(x, reg = 6.toByte) }
    implicit object rdrand2 extends RDRAND_M[r16] { def get(x: r16) = Array[Byte](0x0F.toByte, 0xC7.toByte) ++ modRM(x, reg = 6.toByte) }
    //implicit object rdrand3 extends RDRAND_M[r64] { def get(x: r32) = Array[Byte](0x0F.toByte, 0xC7.toByte) ++ modRM(x, reg = 6.toByte) }
  }
  
 
//}