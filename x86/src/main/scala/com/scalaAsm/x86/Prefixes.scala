package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.GeneralPurpose
import com.scalaAsm.x86.Operands.UniformByteRegister
import com.scalaAsm.x86.Operands._

trait HasRexPrefix[-X] { def get: Array[Byte] }

trait PrefixExtraLow {
  implicit object HasNoPrefix extends HasRexPrefix[Any] { def get = Array() }
}

trait PrefixLow extends PrefixExtraLow {
  implicit object HasRex64 extends HasRexPrefix[GeneralPurpose[_64]] { def get = REX.W(true).get }
}

trait Prefixes extends PrefixLow {
  implicit object HasRexR9 extends HasRexPrefix[R9] { def get = REX(true, true, false, false).get }
  implicit object HasRexR8 extends HasRexPrefix[R8] { def get = REX(true, false, false, true).get }
  implicit object HasRexUniform extends HasRexPrefix[UniformByteRegister[_]] { def get = REX.W(false).get }
}

// 64-bit (R)egister (EX)tension prefix

object REX {
  def W(value: Boolean) = REX(value, false, false, false) // When 1, a 64-bit operand size is used. Otherwise, when 0, the default operand size is used
  def R(value: Boolean) = REX(false, value, false, false) // Extension of the ModR/M reg field
  def X(value: Boolean) = REX(false, false, value, false) // Extension of the SIB index field
  def B(value: Boolean) = REX(false, false, false, value) // Extension of the ModR/M r/m field, SIB base field, or Opcode reg field
}

case class REX(W: Boolean, R: Boolean, X: Boolean, B: Boolean) {
  def get = {
    val prefix: Byte = (64 + 
      (if (W) 8 else 0) +
      (if (R) 4 else 0) +
      (if (X) 2 else 0) +
      (if (B) 1 else 0)).toByte
     Array(prefix)
  }
}