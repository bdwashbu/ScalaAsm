package com.scalaAsm.x86

trait Addressing 
{
  case class *[+A](x: A)
}

  trait Immediate[T, X <: Immediate[_, _]] extends Any {
    def value: T
    def negate: X
  }

  case class Immediate8(val value: Byte) extends AnyVal with Immediate[Byte, Immediate8] {
    def negate: Immediate8 = Immediate8((-value).toByte)
  }

  case class Immediate16(val value: Short) extends AnyVal  with Immediate[Short, Immediate16] {
    def negate: Immediate16 = Immediate16((-value).toShort)
  }

  case class Immediate32(val value: Int) extends AnyVal  with Immediate[Int, Immediate32] {
    def negate: Immediate32 = Immediate32((-value).toInt)
  }

