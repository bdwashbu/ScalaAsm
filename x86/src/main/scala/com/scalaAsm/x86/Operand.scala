package com.scalaAsm.x86

import x86Registers._

object Addressing {

  case class RegisterOffset[+T <: Register, S <: Immediate](offset: S, x: T)

  trait Addressable[X <: Register] {
    self: X =>
    def -[Z <: Immediate {type X = Z }](offset: Z) = RegisterOffset[X, Z](offset.negate, this)
    def +[Z <: Immediate {type X = Z }](offset: Z) = RegisterOffset[X, Z](offset, this)
  }

  case class *[+A](x: A)
  type +[A <: Register, B <: Immediate] = RegisterOffset[A, B]
}

sealed class OperandSize {
  type OpType
}
class ByteOperand extends OperandSize { type OpType = Byte }
class WordOperand extends OperandSize { type OpType = Short }
class DwordOperand extends OperandSize { type OpType = Int }

trait RegisterOrMemory {
  type Size <: OperandSize
  val reg: Register
  val isMemory: Boolean
  val offset8: Immediate
}

trait Operands {
  type imm = Immediate
  type imm8 = Immediate8
  type imm16 = Immediate16
  type imm32 = Immediate32
  
  type rm8 = RegisterOrMemory{ type Size = ByteOperand }
  type rm16 = RegisterOrMemory{ type Size = WordOperand }
  type rm32 = RegisterOrMemory{ type Size = DwordOperand }
  
  type r8 = Register { type Size = ByteOperand }
  type r16 = Register { type Size = WordOperand }
  type r32 = Register { type Size = DwordOperand }
  
 
}

trait Immediate extends Any {
  type immType
  type X <: Immediate
  def value: immType
  def negate: X
  def getBytes: Array[Byte]
}

case class Immediate8(value: Byte) extends AnyVal with Immediate {
  type immType = Byte
  type X = Immediate8
  def negate: Immediate8 = Immediate8((-value).toByte)
  def getBytes: Array[Byte] = Array(value)
}

case class Immediate16(value: Short) extends AnyVal with Immediate {
  type immType = Short
  type X = Immediate16
  def negate: Immediate16 = Immediate16((-value).toShort)
  def getBytes: Array[Byte] = Array((value & 0x00FF).toByte, ((value & 0xFF00) >> 8).toByte)
}

case class Immediate32(val value: Int) extends AnyVal with Immediate {
  type immType = Int
  type X = Immediate32
  def negate: Immediate32 = Immediate32(-value)
  def getBytes: Array[Byte] = Array((value & 0x000000FF).toByte, ((value & 0x0000FF00) >> 8).toByte, ((value & 0x00FF0000) >> 16).toByte, ((value & 0xFF000000) >> 24).toByte)
}

