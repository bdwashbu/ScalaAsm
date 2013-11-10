package com.scalaAsm.x86

import com.scalaAsm.x86._

object Addressing {
  import x86Registers._

  case class RegisterOffset[+T <: Register[_], S <: Immediate[_, _]](offset: S, x: T)

  trait Addressable[X <: Register[_]] {
    self: X =>
    def -[T <: Immediate[_, T]](offset: T) = RegisterOffset[X, T](offset.negate, this)
    def +[T <: Immediate[_, T]](offset: T) = RegisterOffset[X, T](offset, this)
  }

  case class *[+A](x: A)
  type +[A <: Register[_], B <: Immediate[_, _]] = RegisterOffset[A, B]
}

trait Operands {
  type imm8 = Immediate8
  type imm16 = Immediate16
  type imm32 = Immediate32

  import MODRM._

  def modRM[O1, O2](p1: O1, p2: O2, reg: Byte = 0)(implicit ev: MODRM[O1, O2]) = {
    ev.reg = reg
    ev.get(p1, p2)
  }

  def modRM[O1](p1: O1, reg: Byte = 0)(implicit ev: MODRM_1[O1]) = {
    ev.reg = reg
    ev.get(p1)
  }
}

trait Immediate[T, X <: Immediate[_, _]] extends Any {
  def value: T
  def negate: X
}

case class Immediate8(value: Byte) extends AnyVal with Immediate[Byte, Immediate8] {
  def negate: Immediate8 = Immediate8((-value).toByte)
}

case class Immediate16(value: Short) extends AnyVal with Immediate[Short, Immediate16] {
  def negate: Immediate16 = Immediate16((-value).toShort)
}

case class Immediate32(val value: Int) extends AnyVal with Immediate[Int, Immediate32] {
  def negate: Immediate32 = Immediate32((-value).toInt)
}

