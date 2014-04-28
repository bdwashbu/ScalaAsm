package com.scalaAsm.x86

object REX {
  def W(value: Boolean) = REX(value, false, false, false)
  def R(value: Boolean) = REX(false, value, false, false)
  def X(value: Boolean) = REX(false, false, value, false)
  def B(value: Boolean) = REX(false, false, false, value)
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