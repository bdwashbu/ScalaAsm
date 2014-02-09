package com.scalaAsm.x86

import x86Registers._

object Addressing {
  

  case class RegisterOffset[+T <: Register, S <: Immediate[_, _]](offset: S, x: T)

  trait Addressable[X <: Register] {
    self: X =>
    def -[T <: Immediate[_, T]](offset: T) = RegisterOffset[X, T](offset.negate, this)
    def +[T <: Immediate[_, T]](offset: T) = RegisterOffset[X, T](offset, this)
  }

  case class *[+A](x: A)
  type +[A <: Register, B <: Immediate[_, _]] = RegisterOffset[A, B]
}

sealed class OperandSize
class ByteOperand extends OperandSize
class WordOperand extends OperandSize
class DwordOperand extends OperandSize

trait RegisterOrMemory {
  type Size <: OperandSize
  val reg: Register
  val isMemory: Boolean
  val offset8: Byte
}



trait Operands {
  type imm8 = Immediate8
  type imm16 = Immediate16
  type imm32 = Immediate32
  
  type rm8 = RegisterOrMemory{ type Size = ByteOperand }
  type rm16 = RegisterOrMemory{ type Size = WordOperand }
  type rm32 = RegisterOrMemory{ type Size = DwordOperand }
  
  type r8 = Register8
  type r16 = Register16
  type r32 = Register32
  
 
}

trait Immediate[T, X <: Immediate[T, X]] extends Any {
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
  def negate: Immediate32 = Immediate32(-value)
}

