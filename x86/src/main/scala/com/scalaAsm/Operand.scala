package com.scalaAsm.x86

trait Addressing
{
  case class *[+A](x: A)
}

  trait Immediate[T, X <: Immediate[_, _]] extends Any {
    def value: T
    def negate: X
  }

  case class imm8(val value: Byte) extends AnyVal with Immediate[Byte, imm8] {
    def negate: imm8 = imm8((-value).toByte)
  }

  case class imm16(val value: Short) extends AnyVal with Immediate[Short, imm16] {
    def negate: imm16 = imm16((-value).toShort)
  }

  case class imm32(val value: Int) extends AnyVal with Immediate[Int, imm32] {
    def negate: imm32 = imm32((-value).toInt)
  }

