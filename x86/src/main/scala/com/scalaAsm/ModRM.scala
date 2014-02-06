package com.scalaAsm.x86

import com.scalaAsm.utils.Endian
import x86Registers._
import Addressing._

trait ModRMFormat {
  self: Operands =>

  import x86Registers._

  sealed class RegisterMode(val value: Byte)
  case object NoDisplacment extends RegisterMode(0) // If r/m is 110, Displacement (16 bits) is address; otherwise, no displacemen
  case object Displacment8 extends RegisterMode(1)  // Eight-bit displacement, sign-extended to 16 bits
  case object Displacment32 extends RegisterMode(2) // 32-bit displacement (example: MOV [BX + SI]+ displacement,al)
  case object SecondReg extends RegisterMode(3)     // r/m is treated as a second "reg" field

  trait ModRMByte {
    def get: Byte
  }
  
  case class ModRM(mod: RegisterMode, reg: Register, rm: Register) extends ModRMByte {
    def get: Byte = ((mod.value << 6) + (reg.ID << 3) + rm.ID).toByte
  }
  
  case class ModRMExtended(mod: RegisterMode, opEx: Byte, rm: Register) extends ModRMByte {
    def get: Byte = ((mod.value << 6) + (opEx << 3) + rm.ID).toByte
  }

 

  protected[this] trait MODRM_2[-O1, -O2] {
    def get(p1: O1, p2: O2): Array[Byte]
  }

  protected[this] trait MODRM_1[-O1] {
    def get(p1: O1): Array[Byte]
  }

  protected[this] trait MODRM_2Extended[-O1, -O2] {
    def get(p1: O1, p2: O2, opcodeExtension: Byte): Array[Byte]
  }

  protected[this] trait MODRM_1Extended[-O1]  {
    def get(p1: O1, opcodeExtension: Byte): Array[Byte]
  }

  def modRM2[O1, O2](p1: O1, p2: O2)(implicit ev: MODRM_2[O1, O2]) = {
    ev.get(p1, p2)
  }

  def modRM[O1](p1: O1)(implicit ev: MODRM_1[O1]) = {
    ev.get(p1)
  }

  def modRM2Extended[O1, O2](p1: O1, p2: O2, extensionCode: Byte)(implicit ev: MODRM_2Extended[O1, O2]) = {
    ev.get(p1, p2, extensionCode)
  }

  def modRMExtended[O1](p1: O1, extensionCode: Byte)(implicit ev: MODRM_1Extended[O1]) = {
    ev.get(p1, extensionCode)
  }

  implicit object mod1 extends MODRM_2[Register, Register] {
    def get(x: Register, y: Register) = Array(ModRM(SecondReg, x, y).get)
  }

  implicit object mod2 extends MODRM_2[r32, *[r32]] {
    def get(x: r32, y: *[r32]) = Array(ModRM(NoDisplacment, x, y.x).get)
  }

  

  implicit object mod6 extends MODRM_1Extended[rm32] {
    def get(x: rm32, opcodeExtension: Byte) = {
      Array(ModRMExtended(Displacment8, opcodeExtension, x.reg).get, x.offset8)
    }
  }

  implicit object mod8 extends MODRM_1Extended[Register] {
    def get(x: Register, opcodeExtension: Byte) = Array(ModRMExtended(SecondReg, opcodeExtension, x).get)
  }

  implicit object mod7 extends MODRM_2Extended[r32, imm8] {
    def get(x: r32, y: imm8, opcodeExtension: Byte) = Array(ModRMExtended(SecondReg, opcodeExtension, x).get, y.value)
  }

  implicit object mod10 extends MODRM_2Extended[r32, imm32] {
    def get(x: r32, y: imm32, opcodeExtension: Byte) = Array(ModRMExtended(SecondReg, opcodeExtension, x).get) ++ Endian.swap(y.value)
  }

  implicit object mod3 extends MODRM_2[rm32, r32] {
    def get(x: rm32, y: r32) = mod4.get(y, x)
  }
  
  implicit object mod4 extends MODRM_2[r32, rm32] {
    def get(x: r32, y: rm32) = {
      if (y.reg.ID == 4) // [--][--] SIB  
        Array(ModRM(Displacment8, x, y.reg).get, 0x24.toByte, y.offset8)
      else
        Array(ModRM(Displacment8, x, y.reg).get, y.offset8)
    }
  }
}