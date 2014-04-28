package com.scalaAsm.x86

object REX {
  val W = REX(true, false, false, false)
  val R = REX(false, true, false, false)
  val X = REX(false, false, true, false)
  val B = REX(false, false, false, true)
}

case class REX(W: Boolean, R: Boolean, X: Boolean, B: Boolean) {
  def get = {
    val prefix: Byte = (64 + 
      (if (W) 8 else 0) +
      (if (R) 4 else 0) +
      (if (X) 2 else 0) +
      (if (B) 1 else 0)).toByte
     Some(Array(prefix))
  }
}