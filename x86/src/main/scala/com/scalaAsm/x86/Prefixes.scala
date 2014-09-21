package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.GeneralPurpose
import com.scalaAsm.x86.Operands.UniformByteRegister
import com.scalaAsm.x86.Operands._

trait Prefix {
  def get: Array[Byte]
}

// 64-bit (R)egister (EX)tension prefix

//A REX prefix must be encoded when:
//* using 64-bit operand size and the instruction does not default to 64-bit operand size; or
//* using one of the extended registers (R8 to R15, XMM8 to XMM15, YMM8 to YMM15, CR8 to CR15 and DR8 to DR15); or
//* using one of the uniform byte registers SPL, BPL, SIL or DIL.

object REX {
  def W(value: Boolean) = REX(value, false, false, false) // When 1, a 64-bit operand size is used. Otherwise, when 0, the default operand size is used
  def R(value: Boolean) = REX(false, value, false, false) // Extension of the ModR/M reg field
  def X(value: Boolean) = REX(false, false, value, false) // Extension of the SIB index field
  def B(value: Boolean) = REX(false, false, false, value) // Extension of the ModR/M r/m field, SIB base field, or Opcode reg field
}

case class REX(W: Boolean, R: Boolean, X: Boolean, B: Boolean) extends Prefix {
  def get = {
    val prefix: Byte = (64 + 
      (if (W) 8 else 0) +
      (if (R) 4 else 0) +
      (if (X) 2 else 0) +
      (if (B) 1 else 0)).toByte
     Array(prefix)
  }
}