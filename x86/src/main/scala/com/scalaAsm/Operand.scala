package com.scalaAsm.x86

trait Operands {
  case class *[+A](x: A)

  trait Immediate[T, X <: Immediate[_, _]] {
    val value: T
    def negate: X
  }

  case class imm8(val value: Byte) extends Immediate[Byte, imm8] {
    def negate: imm8 = imm8((-value).toByte)
  }

  case class imm16(val value: Short) extends Immediate[Short, imm16] {
    def negate: imm16 = imm16((-value).toShort)
  }

  case class imm32(val value: Int) extends Immediate[Int, imm32] {
    def negate: imm32 = imm32((-value).toInt)
  }
}

